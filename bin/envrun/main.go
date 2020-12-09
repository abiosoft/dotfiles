package main

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"log"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/fatih/color"
	"github.com/joho/godotenv"
)

var envFiles = []string{
	".env.local",
	".env",
}

const (
	usage   = `usage: run <command> [<args>...]`
	logFile = ".run.log"
)

func exitErr(err error) {
	fmt.Fprintf(os.Stderr, "error occured: %v", err)
	fmt.Fprintln(os.Stderr)
	os.Exit(1)
}

func getEnv() map[string]string {

	for _, f := range envFiles {
		stat, err := os.Stat(f)
		if err == nil && !stat.IsDir() {
			m, err := godotenv.Read(f)
			if err != nil {
				err := fmt.Errorf("error loading env file: %w", err)
				log.Fatal(err)
			}
			return m
		}
	}
	return map[string]string{}
}

func openLogFile() (*os.File, error) {
	return os.OpenFile(logFile, os.O_CREATE|os.O_RDWR|os.O_APPEND, 0644)
}

func prepareLogFile() error {
	l, err := openLogFile()
	if err != nil {
		return fmt.Errorf("error preparing log file: %w", err)
	}
	return l.Close()
}

func main() {
	// we only need the time to see changes in output, date not that important
	log.SetFlags(log.Ltime)

	envVars := getEnv()
	args := os.Args[1:]
	if len(args) == 0 {
		exitErr(fmt.Errorf("args missing"))
	}

	if err := prepareLogFile(); err != nil {
		exitErr(fmt.Errorf("error preparing logs: %v", err))
	}

	if args[0] == "log" {
		fmt.Fprintln(os.Stderr, "to view the logs, run the following command or $(run log)")
		fmt.Printf("tail -f %s", logFile)
		fmt.Println()
		return
	}

	cmd, cancel, err := run(args, envVars)
	if err != nil {
		err := fmt.Errorf("error running command: %w", err)
		exitErr(err)
	}

	for {
		kill := func() error {
			return syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
		}
		start := func() {
			cmd, cancel, err = run(args, envVars)
			if err != nil {
				err := fmt.Errorf("error running command: %w", err)
				fmt.Fprintln(os.Stderr, err)
			}
		}

		fmt.Println()
		fmt.Println("input 'r' to restart, 'x' to terminate")
		fmt.Print("  input: ")

		var line string
		fmt.Scanln(&line)

		// extra newline for log clarity
		fmt.Println()

		switch line {
		case "r":
			log.Println("restarting...")
		case "x":
			log.Println("terminating...")
			kill()
			cancel()
			os.Exit(0)
		default:
			log.Printf("unrecognized input '%s'", line)
			log.Println()
			continue
		}

		if cmd == nil {
			log.Println("command not running, cannot restart")
			start()
			continue
		}

		// attempt to kill process and all it's children
		log.Println("attempting to kill process with pid", cmd.Process.Pid)
		if err := kill(); err != nil {
			err := fmt.Errorf("error terminating process: %w", err)
			fmt.Fprintln(os.Stderr, err)
			continue
		}
		log.Println("process killed.")
		log.Println()

		// cancel the process context
		cancel()

		// let's wait for the process in case there's some delay in quitting
		cmd.Process.Wait()

		// let's attempt to run the program again
		start()
	}
}

func run(args []string, vars map[string]string) (*exec.Cmd, func(), error) {
	// convert into shell script for sh to run
	sh := ""
	for _, a := range args {
		sh += " " + strconv.Quote(a)
	}

	// sha args
	cmdArgs := []string{"-c", sh}

	// use a cancel context to be on safe side
	ctx, cancel := context.WithCancel(context.Background())
	cmd := exec.CommandContext(ctx, "sh", cmdArgs...)

	// environment variables
	cmd.Env = os.Environ()
	for k, v := range vars {
		cmd.Env = append(cmd.Env, fmt.Sprintf("%s=%s", k, v))
	}

	// ensure we can kill the children
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}

	// set log outputs
	out, err := openLogFile()
	if err != nil {
		return nil, cancel, fmt.Errorf("error opening log file: %w", err)
	}

	cmd.Stdout = &lineWriter{prefix: color.New(color.BgBlue, color.FgWhite).Sprint("stdout"), out: out}
	cmd.Stderr = &lineWriter{prefix: color.New(color.BgRed, color.FgWhite).Sprint("stderr"), out: out}

	log.Println("running {", strings.Join(args, " "), "}...")
	log.Println()

	if err := cmd.Start(); err != nil {
		return nil, cancel, fmt.Errorf("error starting command: %w", err)
	}

	logStartup(out)

	shutdown := func() {
		cancel()
		logShutdown(out)
		out.Close()
	}

	return cmd, shutdown, nil
}

func logStartup(out io.Writer) {
	fmt.Fprintln(out)
	fmt.Fprintln(out, "starting up at", time.Now())
	fmt.Fprintln(out)
}
func logShutdown(out io.Writer) {
	fmt.Fprintln(out)
	fmt.Fprintln(out, "shutting down at", time.Now())
	fmt.Fprintln(out)
}

var _ io.Writer = (*lineWriter)(nil)

// lineWriter is a simple writer that only writes to an underlying writer when
// a newline is encountered.
type lineWriter struct {
	out    io.Writer
	prefix string

	buf bytes.Buffer
	sync.Mutex
}

func (l *lineWriter) Write(b []byte) (int, error) {
	l.Lock()
	defer l.Unlock()

	for i := 0; i < len(b); i++ {

		// special case: replace escaped chars with their real value
		// newline, tab, quote, backslack
		if b[i] == '\\' {
			// peek if available
			if i+1 < len(b) {
				i++
				switch b[i] {
				case 'n':
					b[i] = '\n'
				case 't':
					b[i] = '\t'

				// do nothing for these, escape char already skipped
				case '\\':
				case '"':
				case '\'':

				// otherwise, don't skip escape char
				default:
					i--
				}
			}
		}

		// cache the char
		l.buf.WriteByte(b[i])

		// write to underlying writer if newline is encountered
		if b[i] == '\n' {
			l.out.Write([]byte(l.prefix + " "))
			l.buf.WriteTo(l.out)
			l.buf.Truncate(0)
		}
	}

	// all bytes are always successfully written
	return len(b), nil
}
