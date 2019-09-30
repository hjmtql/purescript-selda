# Simple End-to-End Example

```purescript
module Guide.SimpleE2E where

import Prelude

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Database.PostgreSQL as PostgreSQL
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Effect.Class.Console (log, logShow)
import Selda (class MonadSelda, Col, FullQuery, Table(..), aggregate, count, groupBy, insert_, leftJoin, lit, notNull, query, restrict, selectFrom, selectFrom_, showQuery, (.==), (.>))
```

First we have to setup the database.
We create a db called *purspg* and a user.
We include this information in a record below.
We will need it later to execute our queries.

```purescript
dbconfig ∷ PostgreSQL.PoolConfiguration
dbconfig = (PostgreSQL.defaultPoolConfiguration "purspg")
  { user = Just "init"
  , password = Just $ "qwerty"
  , idleTimeoutMillis = Just $ 1000
  }
```

Now we should create some tables in our database.
We will use the `postgresql-client` to get the job done.
To do so we define an auxiliary function `execute` that takes the SQL string literal, executes it and if something went wrong throws an error.

```purescript
execute ∷ String → PostgreSQL.Connection → Aff Unit
execute sql conn = do
  PostgreSQL.execute conn (PostgreSQL.Query sql) PostgreSQL.Row0
    >>= maybe (pure unit) (throwError <<< error <<< show)
```

The function that creates our first table - `people` - is defined below.

```purescript
createPeople ∷ PostgreSQL.Connection → Aff Unit
createPeople = execute """
  DROP TABLE IF EXISTS people;
  CREATE TABLE people (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    age INTEGER
  );"""
```

To represent this table definition in PureScript, we use a `Table` type which is parameterized by a *row of types* that describes the columns and their types in the database.

Please note that `purescript-selda` does not handle schema modification, such as table creation, we are doing it manually using `postgresql-client`.
So it is important to correctly define the tables and types for its columns.

Notice that `name` column has `NOT NULL` constraint, unlike the `age` column.
We use `Maybe` for type of nullable columns.

```purescript
people ∷ Table 
  ( id ∷ Int
  , name ∷ String
  , age ∷ Maybe Int
  )
people = Table { name: "people" }
```

Similarly we create a second table - `bankAccounts`.

```purescript
createBankAccounts ∷ PostgreSQL.Connection → Aff Unit
createBankAccounts = execute """
  DROP TABLE IF EXISTS bank_accounts;
  CREATE TABLE bank_accounts (
    id INTEGER PRIMARY KEY,
    personId INTEGER NOT NULL,
    balance INTEGER NOT NULL
  );"""

bankAccounts ∷ Table
  ( id ∷ Int
  , personId ∷ Int
  , balance ∷ Int
  )
bankAccounts = Table { name: "bank_accounts" }
```

## First Query

Since we have defined table definitions, we can write some queries.
Let's start with a simple `LEFT JOIN` query with some arbitrary condition in the `WHERE` clause.

```purescript
qNamesWithBalance
  ∷ ∀ s. FullQuery s { name ∷ Col s String , balance ∷ Col s (Maybe Int) }
qNamesWithBalance = 
  selectFrom people \{ id, name, age } → do
    { balance } ← leftJoin bankAccounts \acc → id .== acc.personId
    restrict $ id .> lit 1
    pure { name, balance }
```

And below is the generated SQL for the query `qNamesWithBalance`.

  ```sql
  SELECT people_0.name AS name, bank_accounts_1.balance AS balance
  FROM people people_0
  LEFT JOIN bank_accounts bank_accounts_1 ON ((people_0.id = bank_accounts_1.personId))
  WHERE (people_0.id > 1)
  ```

We define a query using the `selectFrom` function by providing a table definition and a function that takes a record of columns from the table and returns a *query description*.
Operations such as `restrict` and `leftJoin` modify the state of the query, or as we called it earlier - *query description*.

<!-- For example: the `leftJoin` call in the query above adds the `LEFT JOIN` clause.
At the end we pick which columns should be included in the result. -->

<!-- First let's look at the line with the `restrict` call.
We want to query only these people that have `id` greater then `1`.
To express this we use the operator `.>` defined by `selda` and we precede `1` by calling `lit`, because we are dealing with abstract values (of type `Col s a`) that represent database values like columns, literals, expressions, etc. -->

Notice that `leftJoin` also changes the types in a column's record. The `balance` column is nullable in that context, so it represents a value of type `Maybe Int`.

## Nested Query

We can use the previously defined `qNamesWithBalance` as a subquery to filter out the null values in the `balance` column.

```purescript
qBankAccountOwnersWithBalance
  ∷ ∀ s. FullQuery s { name ∷ Col s String , balance ∷ Col s Int }
qBankAccountOwnersWithBalance = 
  selectFrom_ qNamesWithBalance \{ name, balance: nullableBalance } → do
    balance ← notNull nullableBalance
    pure { name, balance }
```

We used the `selectFrom_` function which is similar to the `selectFrom` that we saw earlier, but instead of *table definition* we provide a nested query as its first argument. 

In the *query description* we filter out the null values in balance.
It adds to the `WHERE` clause that `balance IS NOT NULL` and it returns a column representation `Col s Int` instead of `Col s (Maybe Int)`.

## Aggregation

We would like to know how many people have a bank account.
To do this we first write a query that returns `personId` from `bankAccounts` without duplicates.
We accomplish that using aggregation: we are groupping by `personId` column and simply return the only aggregated column.

Queries that use aggregation can be problematic.
Only aggregated columns and results of aggregate functions can appear in the result.
To prevent some such runtime errors, we added separate representation for aggregate values (`Aggr s a`) which is only returned by `groupBy` and aggregate functions like `count` and `max_`.
Mixing `Col` and `Aggr` is not allowed and it will result in a type error.
To validate and use the query (nest it or execute it) we have to call `aggregate` function that changes the `Aggr` into `Col`.

```purescript
qCountBankAccountOwners
  ∷ ∀ s. FullQuery s { numberOfOwners ∷ Col s String }
qCountBankAccountOwners = 
  aggregate $ selectFrom_
    (aggregate $ selectFrom bankAccounts \{ id, personId } → do
      pid ← groupBy personId
      pure { pid })
    \{ pid } → pure { numberOfOwners: count pid }
```

## Type Errors

Sometimes we do something wrong and it (hopefully) results in a type error (and not a runtime error).
We would like to get useful error messages that lead us to the source of the problem, but when a library heavily uses generic programming on type classes it is not always possible...

In the example query `qCountBankAccountOwners` above, when we forget the `aggregate` call, use `personId` instead of `pid` in the result or include `id` in the result we will get following error message:

  ```
  No type class instance was found for Heterogeneous.Mapping.Mapping OuterCols t4 t5
  The instance head contains unknown type variables. Consider adding a type annotation
  ```

Without knowing some implementation details this message is not really helpful.
We can mitigate the problem with these error messages, by providing a type annotation for the nested query or define it as top-level value.

```purescript
qBankAccountOwnerIds ∷ ∀ s. FullQuery s { pid ∷ Col s Int }
qBankAccountOwnerIds = 
  aggregate $ selectFrom bankAccounts \{ id, personId } → do
    pid ← groupBy personId
    pure { pid }
```

Now if we use `id` or `personId` in the result we will get custom error message in the line with `aggregate` call: `field 'pid' is not aggregated. Its type should be 'Aggr _ _'`.
Above error message will appear even without the type annotation.

## Execution

Now we will show how to execute queries and perform insert operations using `purescript-selda`.
We perform these actions in a monad that satisfies three constraints: `MonadAff m, MonadError PGError m, MonadReader PostgreSQL.Connection m`.
There is a provided shortcut for these classes called `MonadSelda m`.

If your context in a `Reader` or the error type in `Except` are different then it is necessary to write a *hoist* function that transforms `MonadSelda` to your monad stack.

```purescript
app ∷ ∀ m. MonadSelda m ⇒ m Unit
app = do
  insert_ people
    [ { id: 1, name: "name1", age: Just 11 }
    , { id: 2, name: "name2", age: Just 22 }
    , { id: 3, name: "name3", age: Just 33 }
    ]
  insert_ bankAccounts
    [ { id: 1, personId: 1, balance: 100 }
    , { id: 2, personId: 1, balance: 150 }
    , { id: 3, personId: 3, balance: 300 }
    ]
```
Let's start with some insert operations, so we have something in the database to work with.
```purescript

  log $ showQuery qNamesWithBalance
  query qNamesWithBalance >>= logShow
```
We execute a query by calling `query` and as a result we get an array of records.
We can also get SQL string literal from a query using `showQuery` function.
```purescript
  log $ showQuery qBankAccountOwnersWithBalance
  query qBankAccountOwnersWithBalance >>= logShow

  log $ showQuery qCountBankAccountOwners
  query qCountBankAccountOwners >>= logShow
```

Now we will finally write the `main` that will interpret our `app`.
We start with getting the database connection ready.

```purescript
main ∷ Effect Unit
main = do
  pool ← PostgreSQL.newPool dbconfig
  launchAff_ do
    PostgreSQL.withConnection pool case _ of
      Left pgError → logShow ("PostgreSQL connection error: " <> show pgError)
      Right conn → do
```
When we've got the connection we can create the database tables and then run our monad stack.
We are going to wrap everything in a transaction and do a rollback at the end.
```purescript
        execute "BEGIN TRANSACTION" conn

        createPeople conn
        createBankAccounts conn

        runReaderT (runExceptT app) conn >>= either logShow pure

        execute "ROLLBACK TRANSACTION" conn
```