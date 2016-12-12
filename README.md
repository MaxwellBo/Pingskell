# Pingskell

My solution to [TandaHQ/work-samples/pings](https://github.com/TandaHQ/work-samples/tree/master/pings%20(backend)). 

## Requirements

- [Stack](https://docs.haskellstack.org/en/stable/README/) (`curl -sSL https://get.haskellstack.org/ | sh`)

- [Docker](https://www.docker.com/products/overview) (`brew cask install docker` or `sudo apt-get install docker-engine`) OR [Redis](https://redis.io/) (`brew install redis` or `sudo apt-get install redis-server`)

## Usage

#### Booting the DB

1. `docker pull redis`
2. `docker run --name PingskellDB -d -p 6379:6379 redis --appendonly yes` 

OR 

1. `redis --appendonly yes`

#### Booting the server

1. `stack build && stack exec Pingskell`

#### Running the tests

1. `ruby pings.rb`

#### Tearing down the DB

1. `docker kill PingskellDB`
