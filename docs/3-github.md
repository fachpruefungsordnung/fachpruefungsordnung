# GitHub Actions

The repository contains GitHub actions to test and deploy the code. The actions
are configured to run on self-hosted runners. We used the
[Docker runners from myoung34](https://github.com/myoung34/docker-github-actions-runner)
to run the actions. Deployment happens via SSH to two different machines (a dev environment
and a production enviroment, which is only deployed to on tagged commits on `main`).
The setup works, although it could be improved to use a Docker container registry
in the future.

## Secrets

To get the actions up and running, there are a few secrets and variables to set up:

### Deployment server secrets

The current `main` branch is, pending passing tests, autodeployed to the dev server
via SSH. To get this to work, you'll need the following secrets.

- `DEV_REMOTE_HOST`: The hostname of the development server to SSH into
- `DEV_REMOTE_PASSWORD`: The development server SSH password
- `DEV_REMOTE_USER` The development server SSH user
- `DEV_SSH_PORT`: The development server SSH port
- `DEV_SERVER_HOST`: The URL the development server will be reachable at

Similar properties exist for the production deployment server, namely:

- `REMOTE_HOST`
- `REMOTE_PASSWORD`
- `REMOTE_USER`
- `SSH_PORT` and
- `SERVER_HOST`

Deployment to the production server will only happen with commits on `main` tagged
with the `prod` tag. Use the `scripts/deploy-prod` script to quickly trigger a deployment.

### Mail configuration settings

These secrets are needed if you want to use the mail service within the backend. These
are currently supplied by the action, but could also be passed via the `.env` file.

- `MAIL_ADDRESS`
- `MAIL_HOST`
- `MAIL_PASSWORD`
- `MAIL_PORT`
- `MAIL_USERNAME`
