# sortops_erl

Simple Erlang HTTP service used for creation proper Linux task execution order based on inter-task dependencies. Built on top of [NOVA Framework](https://www.novaframework.org/).
## How it works
Service is operating through simple REST-API. As an output it returns either correctly ordered JSON object, or bash script with correctly ordered commands. The Application logic will be described in next sections.
### Input and output data format
The input JSON object is expected to be as follows:
```json
 {"tasks":
    [
      {
        "name": "task_1",
        "command": "ls -l",
        "requires": ["task_2"]},
      {
        "name": "task_2",
        "command": "echo 'Hello, World!'"
      },
      {
        "name": "task_3",
        "command": "touch /tmp/file.txt",
        "requires":["task_1"]
      }
 ]}
```
Here you can see, that dependencies for a task are specified via **"requires"** key. And this shows that our sorting problem is [**topological sorting**](https://en.wikipedia.org/wiki/Topological_sorting) problem. This is very convenient, since Erlang provides solution almost [out of box](https://rosettacode.org/wiki/Topological_sort#Erlang). The output JSON object is expected to look as follows:
```json
 {
  "tasks":
    [
      {
        "name": "task_2",
        "command": "echo 'Hello, World!'"
      },
      {
        "name": "task_1",
        "command": "ls -l"
      },
      {
        "name": "task_3",
        "command": "touch /tmp/file.txt"
      }
  ]
  }
```
and bash script is expected to be like:
```bash
#!/usr/bin/env bash
echo 'Hello, World!
ls -l
touch /tmp/file.txt
```
The return format for bash script is
```json
{"script": "(your bash script)"}
```
### Input Data validation
Due to specific impelenetation of Topological search in Erlang (it uses ETS tables under the hood) and well-defined structure of the input data, it's more effective to use JSON schemas for input data validation. The [NOVA JSON schemas plugin]("https://github.com/novaframework/nova_json_schemas.git") is used in this implementation for data validation. The schema for data valiadation can be found in **"priv/schemas"**.

### HTTP API
In order to retrieve data described in [Input and output data format subsection](#input-and-output-data-format) the following requests are needed:
```
 _ /api (sortops_erl, sortops_erl_api_controller:index/1)
         ├─  POST /sort_to_script - endpoint for bash script
         └─  POST /sort_job - endpoint for sorted tasks
```

## Installation, setup and testing
### Prerequisites
Before going to following sections, ensure that you have installed all prerequisites:
* Erlang/OTP 28.0.1
* Rebar3 3.24.0

### Development mode
Since project is in active development, all potential contributors can play with it in development mode. To install and use the application in development mode, you can do the following:
```bash
# Clone project source code
git clone git@github.com:nilforms/sortops_erl.git
# Move to local project directory
cd sortops_erl
# Install project dependencies
rebar3 get-deps
# Start project 
rebar3 nova serve
```
The last command will open the iex shell with all the application modules loaded. So this is useful when you would like to test function manually.
If you would like to launch tests, after installing the dependencies, execute:
```bash
rebar3 eunit # tests sorting algorithm
rebar3 ct # tests for API
```
### Release
As mentioned in [Mix documentation](https://hexdocs.pm/mix/1.17.3/Mix.Tasks.Release.html) you can create and launch a new release for production-like use.
To do this, after dependencies installation step from [previous subsection](#development-mode), execute the following:
```bash
# Create new release
rebar3 as prod release -o $PATH_TO_RELEASE
# Move to release directory
cd  $PATH_TO_RELEASE
# Start the release
bin/sortops_erl daemon
```
### Configuration
Currently, applications default tcp port can be configured.

for production you can set them up using Linux env variables:
```bash
 PORT={your_port} path_to_release/bin/sortops_erl start
```