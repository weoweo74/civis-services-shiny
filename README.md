# Civis Services Docker Image for R
[![CircleCI](https://circleci.com/gh/civisanalytics/civis-services-shiny/tree/master.svg?style=svg)](https://circleci.com/gh/civisanalytics/civis-services-shiny/tree/master)

# Installation

Either build the Docker image locally
```bash
docker build -t civis-services-shiny .
```

or download the image from DockerHub
```bash
docker pull civisanalytics/civis-services-shiny:latest
```

The `latest` tag (Docker's default if you don't specify a tag)
will give you the most recently built version of the civis-services-shiny
image. You can replace the tag `latest` with a version number such as `1.0`
to retrieve a reproducible environment.

# Testing Integration with the Civis Platform

If you would like to test the image locally follow the steps below:

1. Have an application available for testing.  If you don't have one handy, you can use the official example app:
   ```
   git clone https://github.com/rstudio/shiny_example
   ```
2. Build your image locally:
   ```
   docker build -t civis-services-shiny:test .
   ```
3. Run the container:
   ```
   docker run --rm -p 3838:3838 -e APP_DIR=/app civis-services-shiny:test
   ```

   This mounts the `shiny_example` folder in the Docker container under `/app`, where the entrypoint expects to find it.  You will need to modify the run command if your application is at a different path.

4. Access the app at the ip of your docker host with port 3838:
   ```
   <docker-host-ip>:3838
   ```

For example, when using Docker for Mac `<docker-host-ip>` was `127.0.0.1`.

# Contributing

See [CONTRIBUTING](CONTRIBUTING.md) for information about contributing to this project.

If you make any changes, be sure to build a container to verify that it successfully completes:
```bash
docker build -t civis-services-shiny:test .
```
and describe any changes in the [change log](CHANGELOG.md).

## For Maintainers

This repo has autobuild enabled. Any PR that is merged to master will be built as the `latest` tag on Docker Hub.

### Creating a new release

Once you are ready to create a new version, go to the "releases" tab of the repository and click
"Draft a new release". Github will prompt you to create a new tag, release title, and release
description. The tag should use semantic versioning in the form "vX.X.X"; "major.minor.micro".

The title of the release should be the same as the tag. Include a change log in the release description.

Once the release is tagged, DockerHub will automatically build three identical containers, with labels
"major", "major.minor", and "major.minor.micro".

### Test branches

This repo has branch builds enabled.  Branches will be built with the tag  `dev-<branch name>` on Docker Hub.


# License

BSD-3

See [LICENSE.md](LICENSE.md) for details.
