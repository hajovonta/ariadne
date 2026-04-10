# Graph Events & Webhooks

Event hooks that fire on graph mutations, with optional HTTP webhook delivery via Drakma.

## `on-graph-event`

```lisp
(on-graph-event graph name &key event callback url)
```

Register an event hook. EVENT can be `:add`, `:remove`, or `:all`. Provide CALLBACK (a function receiving an event plist) and/or URL (a webhook endpoint that receives a JSON POST).

### Examples

```lisp
;; Local callback
(on-graph-event g "logger" :event :add
  :callback (lambda (evt) (format t "Added: ~A~%" (getf evt :subject))))

;; Webhook
(on-graph-event g "notify" :event :all
  :url "http://localhost:8080/webhook")

;; Both
(on-graph-event g "both" :event :remove
  :callback (lambda (evt) (log-event evt))
  :url "http://example.com/hook")
```

## `remove-graph-event`

```lisp
(remove-graph-event graph name)
```

Unregister an event hook by name.

## `graph-events`

```lisp
(graph-events graph)
```

Return the list of registered event hooks.

## `event-to-json`

```lisp
(event-to-json event-plist)
```

Serialize an event plist to a JSON string.

### Example

```lisp
(event-to-json '(:event :add :subject "http://ex.org/s" :predicate "http://ex.org/p" :object "http://ex.org/o"))
;; => "{\"event\":\"add\",\"subject\":\"http://ex.org/s\",\"predicate\":\"http://ex.org/p\",\"object\":\"http://ex.org/o\"}"
```

## Event Plist Format

Events passed to callbacks contain:

| Key | Value |
|-----|-------|
| `:event` | `:add` or `:remove` |
| `:subject` | Triple subject |
| `:predicate` | Triple predicate |
| `:object` | Triple object |
