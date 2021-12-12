# invoker

A tool for sending messages to AWS queues. Allows you to summon your most used
messages via templates.

## Install  

Run `stack install`

## Run

`invoker` will automatically read configuration from `./.invoker` if available.
Put your default queue URL in `.invoker/queue` and your template files in
`.invoker/templates`. The file names are the names of the templates and the
content is automatically read.
