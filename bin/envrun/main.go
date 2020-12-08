package main

import (
	"context"
	"fmt"
	"log"
	"os"
	"os/exec"
	"strconv"
	"syscall"

	"github.com/joho/godotenv"
)

var envFiles = []string{
	".env.local",
	".env",
}

const (
	configDir  = ".run"
	stdoutFile = ".run/stdout.log"
	stderrFile = ".run/stderr.log"
)

func exitErr(err error) {
	fmt.Fprintf(os.Stderr, "error occured: %v", err)
	fmt.Fprintln(os.Stderr)
	os.Exit(1)
}

const usage = `usage: run <command> [<args>...]`

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

func prepareLogs() error {
	if err := os.MkdirAll(configDir, 0755); err != nil {
		return err
	}
	return nil
}

func main() {
	envVars := getEnv()
	args := os.Args[1:]
	if len(args) == 0 {
		exitErr(fmt.Errorf("args missing"))
	}

	if args[0] == "log" {
		fmt.Fprintln(os.Stderr, "to view the logs, run the following command or $(run log)")
		fmt.Printf("tail -f %s %s", stdoutFile, stderrFile)
		fmt.Println()
		return
	}

	if err := prepareLogs(); err != nil {
		exitErr(fmt.Errorf("error preparing logs: %v", err))
	}

	cmd, cancel, err := run(args, envVars)
	if err != nil {
		err := fmt.Errorf("error running command: %w", err)
		exitErr(err)
	}

	for {
		kill := func() error { return syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL) }
		fmt.Println("input 'r' to restart, 'x' to terminate")
		fmt.Print("  your input: ")

		var line string
		fmt.Scanln(&line)

		switch line {
		case "r":
			fmt.Println("restarting...")
		case "x":
			fmt.Println("terminating...")
			kill()
			os.Exit(0)
		default:
			fmt.Printf("unrecognized input '%s'", line)
			fmt.Println()
			continue
		}

		if cmd == nil {
			fmt.Fprintln(os.Stderr, "command not running, cannot restart")
			continue
		}

		// attempt to kill process and all it's children
		fmt.Fprintln(os.Stderr, "attempting to kill process with pid", cmd.Process.Pid)
		if err := kill(); err != nil {
			err := fmt.Errorf("error terminating process: %w", err)
			fmt.Fprintln(os.Stderr, err)
			continue
		}
		fmt.Fprintln(os.Stderr, "process killed.")
		fmt.Fprintln(os.Stderr)

		// cancel the process context
		cancel()

		// let's wait for the process in case there's some delay in quitting
		cmd.Process.Wait()

		// start process again
		cmd, cancel, err = run(args, envVars)
		if err != nil {
			err := fmt.Errorf("error running command: %w", err)
			fmt.Fprintln(os.Stderr, err)
		}

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
	stdout, err := os.OpenFile(stdoutFile, os.O_CREATE|os.O_RDWR|os.O_APPEND, 0644)
	if err != nil {
		return nil, cancel, fmt.Errorf("error preparing stdout: %w", err)
	}
	defer stdout.Close()

	stderr, err := os.OpenFile(stderrFile, os.O_CREATE|os.O_RDWR|os.O_APPEND, 0644)
	if err != nil {
		return nil, cancel, fmt.Errorf("error preparing stderr: %w", err)
	}
	defer stderr.Close()

	cmd.Stdout = stdout
	cmd.Stderr = stderr

	if err := cmd.Start(); err != nil {
		return nil, cancel, fmt.Errorf("error starting command: %w", err)
	}

	return cmd, cancel, nil
}
