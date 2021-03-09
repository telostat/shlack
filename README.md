# shlack - Send Slack messages from Shell

![GitHub release (latest by date)](https://img.shields.io/github/v/release/telostat/shlack)
![GitHub contributors](https://img.shields.io/github/contributors/telostat/shlack)
![GitHub](https://img.shields.io/github/license/telostat/shlack)

## Installation

Find pre-compiled binaries under the releases page.

## Usage

```
$ shlack --help
shlack - Send Slack messages from shell, quickly

Usage: shlack [--version] COMMAND
  shlack

Available options:
  -h,--help                Show this help text
  --version                Show version

Available commands:
  send                     Sends Slack message
```

... or in particular for sending:

```
$ shlack send --help
Usage: shlack send [-c|--config CONFIG] [-p|--profile PROFILE]
                   [-h|--header HEADER] [-b|--body BODY] [-f|--footer FOOTER]
  Sends Slack message

Available options:
  -c,--config CONFIG       Shlack configuration file (YAML or JSON)
  -p,--profile PROFILE     Profile name to use, defaults to first profile in
                           config
  -h,--header HEADER       Header text
  -b,--body BODY           Body text
  -f,--footer FOOTER       Footer text
  -h,--help                Show this help text
```

Example:

```
shlack send --header "This is the header" --body "This is the body" --footer "This is the footer"
```

You can add a configuration file at one the locations for automatic lookup in
the following order:

1. `${PWD}/.shlack.json`
1. `${PWD}/.shlack.yaml`
1. `${HOME}/.shlack.json`
1. `${HOME}/.shlack.yaml`
1. `/etc/shlack.json`
1. `/etc/shlack.yaml`

... or specify a configuration file with `-c/--config` option.

The configuration file reads like this:

```
profiles:
  - name: example-1
    webhook: <SLACK-WEBHOOK-URL-FOR-EXAMPLE-1>
  - name: example-2
    webhook: <SLACK-WEBHOOK-URL-FOR-EXAMPLE-2>
```

You can choose a profile from the available ones in the configuration file.
Profile can be specified with `-p/--profile` option, but if not provided,
defaults to the first profile.

## License

Copyright Telostat Pte Ltd (c) 2021.

This work is licensed under BSD3. See [LICENSE](./LICENSE).
