# SQamL installation instructions

Welcome to SQamL, an OCaml-based mini SQL database! This document will guide you through the installation process of our software.

## Prerequisites

Before installing SQamL, you need to have the following software installed on your machine:

-   [OCaml](https://ocaml.org/docs/install.html)
-   [Dune](https://dune.build/)
-   [Git](https://git-scm.com/)

For instructions on how to install and setup the above, please refer to the links provided.

## Installation

SQamL is available [on GitHub here](https://github.com/Destaq/sqaml/). Please clone it to your local machine by running the following command:

```bash
git clone https://github.com/Destaq/sqaml.git
```

After cloning the repository, navigate to the project directory:

```bash
cd sqaml
```

You can now build the project using Dune:

```bash
dune build
```

Finally, you can run `sqaml` by executing the following command:

```bash
dune exec sqaml
```

In the command-line, that should generate the following output:

```text
  _oo\
  (__/ \  _  _
    \  \/ \/ \
    (         )\
      \_______/  \
      [[] [[]]
      [[] [[]]
Welcome to the SQAMLVerse!
Enter an SQL command (or 'exit;' to quit):
```

## Documentation

We currently support the following SQL commands/clauses using regular MySQL syntax.

-   `CREATE TABLE`
-   `INSERT INTO`
-   `SELECT`
-   `SHOW TABLES`
-   `DELETE FROM`
-   `UPDATE`
-   `SET`
-   `DROP TABLE`
-   `WHERE`
-   `LIMIT`
-   `ORDER BY`


Supported data types are:

-   `INT`
-   `VARCHAR`

(and `PRIMARY KEY` which is not technically a type)

## Functionality demonstration

Please note that all SQL commands must be terminated with a semicolon (`;`). Additionally, **equality conditions (i.e. `WHERE x = y` clauses) are only supported with a space around the `=`**.

```text
  _oo\
  (__/ \  _  _
    \  \/ \/ \
    (         )\
      \_______/  \
      [[] [[]]
      [[] [[]]
Welcome to the SQAMLVerse!
Enter an SQL command (or 'exit;' to quit): CREATE TABLE users (id int primary key, name varchar);
id: int
name: varchar

Enter an SQL command (or 'exit;' to quit): CREATE TABLE users (id int primary key, name varchar, age int);
Error: Table already exists

Enter an SQL command (or 'exit;' to quit): INSERT INTO users (id, name) VALUES (1, 'Simon');
id: int
name: varchar
1 'Simon'

Enter an SQL command (or 'exit;' to quit): INSERT INTO users (id, name) VALUES (2, 'Alex');
id: int
name: varchar
2 'Alex'
1 'Simon'

Enter an SQL command (or 'exit;' to quit): SELECT * FROM users;
2 'Alex'
1 'Simon'

Enter an SQL command (or 'exit;' to quit): SELECT * FROM users WHERE name = 'Alex';
2 'Alex'

Enter an SQL command (or 'exit;' to quit): SELECT * FROM users ORDER BY id ASC;
1 'Simon'
2 'Alex'

Enter an SQL command (or 'exit;' to quit): SELECT * FROM users ORDER BY id LIMIT 1;
1 'Simon'

Enter an SQL command (or 'exit;' to quit): DELETE FROM users WHERE name = 'Alex';

Enter an SQL command (or 'exit;' to quit): UPDATE users SET name = 'Clarkson' WHERE id = 1;

Enter an SQL command (or 'exit;' to quit): SELECT * FROM users;
1 'Clarkson'

Enter an SQL command (or 'exit;' to quit): SHOW TABLES;
Tables:
users

Enter an SQL command (or 'exit;' to quit): DROP TABLE users;

Enter an SQL command (or 'exit;' to quit): SHOW TABLES;
No tables in database.
```
Tables are persisted upon typing `exit;`, and the saved files for each corresponding table can be found in the `lib/storage` directory. For example, if a table `users` is created, the table will be saved to the path `lib/storage/users.sqaml`.
## Tests

To run test, you can run `make bisect` from the root directory. This will run the tests and generate a coverage report in the `_coverage` directory, which is automatically opened.

You can also choose to directly run the tests through `dune test`, also from the root directory.

```bash
dune test
```

Please note that our tests require a Unix-like environment to run.

## Addendum

Please note that this is a work-in-progress and so there may still be some bugs and missing features. We are actively working on making SQamL the best it can be, though, and anticipate a full release soon.
