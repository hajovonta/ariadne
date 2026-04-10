# Backup & Restore

Timestamped versioned snapshots with restore.

## `backup-graph`

```lisp
(backup-graph graph directory)
```

Save a timestamped backup of the graph to DIRECTORY. Returns the backup file path.

## `restore-latest-backup`

```lisp
(restore-latest-backup graph directory)
```

Restore the most recent backup from DIRECTORY into GRAPH.

## `list-backups`

```lisp
(list-backups directory)
```

Return a list of backup file paths in DIRECTORY, sorted by timestamp.

### Example

```lisp
(backup-graph g "/tmp/backups/")
;; => "/tmp/backups/mydb-20260410-083000.bak"

(list-backups "/tmp/backups/")
;; => ("/tmp/backups/mydb-20260409-120000.bak" "/tmp/backups/mydb-20260410-083000.bak")

(restore-latest-backup g "/tmp/backups/")
```
