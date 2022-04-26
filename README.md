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

## Key bindings

### Open template screen (`ctrl + t`)

Pick a template to insert into the message field. The templates are automatically read from your
`.invoker/templates` directory and should be `.json` files.

### Send message (`ctrl + s`)

Sends the content in your message field to the queue.

### Clear queue / delete messages (`ctrl + d`)

Purges your queue.

### Open menu (`F2`)

Opens a menu that can be used to quit the application or open the help menu (for key binding help).

### Quit (`ctrl + q`)

Quits the application.

### Close current view (`Escape`)

Closes both the template view and the menu.
