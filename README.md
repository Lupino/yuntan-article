# Article micro server

This is the core source of huabot.com.

## Build

```bash
git clone https://github.com/Lupino/yuntan-article.git
cd yuntan-article
stack build
stack install
```

## Running

First create a mysql database.

```bash
$ mysql -u root
mysql> CREATE DATABASE yuntan_article;
mysql> exit;
```

Second update the config.yaml, change the database name to you desire one.

```bash
cp config.sample.yaml config.yaml
vim config.yaml
```

Third running.

```bash
yuntan-article -c config.yaml
```

## API

See [docs](https://lupino.github.io/yuntan-article/)
