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
	if err := os.MkdirAll(".run", 0755); err != nil {
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

	if err := prepareLogs(); err != nil {
		exitErr(fmt.Errorf("error preparing logs: %v", err))
	}

	cmd, cancel, err := run(args, envVars)
	if err != nil {
		err := fmt.Errorf("error running command: %w", err)
		exitErr(err)
	}

	for {
		fmt.Println("input 'r' to restart")
		fmt.Println()

		var line string
		fmt.Scanln(&line)

		if line != "r" {
			continue
		}

		if cmd == nil {
			fmt.Fprintln(os.Stderr, "command not running, cannot restart")
			continue
		}

		cmd.Process.Signal(os.Interrupt)
		cmd.Process.Signal(os.Kill)
		pgid, err := syscall.Getpgid(cmd.Process.Pid)
		syscall.Kill(pgid, syscall.SIGTERM)

		// attempt to kill process
		fmt.Fprintln(os.Stderr, "attempting to kill process with pid", cmd.Process.Pid)
		if err := cmd.Process.Kill(); err != nil {
			err := fmt.Errorf("error terminating process: %w", err)
			fmt.Fprintln(os.Stderr, err)
			continue
		}

		cancel()

		// start process again
		cmd, cancel, err = run(args, envVars)
		if err != nil {
			err := fmt.Errorf("error running command: %w", err)
			fmt.Fprintln(os.Stderr, err)
		}

	}
}

func run(args []string, vars map[string]string) (*exec.Cmd, func(), error) {
	sh := ""
	for _, a := range args {
		sh += " " + strconv.Quote(a)
	}
	cmdArgs := []string{"-c", sh}
	ctx, cancel := context.WithCancel(context.Background())
	cmd := exec.CommandContext(ctx, "sh", cmdArgs...)
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	cmd.Env = os.Environ()
	for k, v := range vars {
		cmd.Env = append(cmd.Env, fmt.Sprintf("%s=%s", k, v))
	}

	stdout, err := os.OpenFile(".run/stdout.log", os.O_CREATE|os.O_RDWR|os.O_APPEND, 0644)
	if err != nil {
		return nil, cancel, fmt.Errorf("error preparing stdout: %w", err)
	}
	defer stdout.Close()

	stderr, err := os.OpenFile(".run/stderr.log", os.O_CREATE|os.O_RDWR|os.O_APPEND, 0644)
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
