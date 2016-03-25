# Working with the database.

Data invariants cannot themselves be enforced using the Persistent Entity Syntax
as seen in `config/models`. Though less certifiably safe, we can still be
stringent about enforcing any invariants in our Model code.

## When to enforce invariants within code:

Only check to make sure any data invariants are not violated when performing
*create* or *update* actions; *deletes* and *reads* do ***not*** require
invariant checking, as those should have been enforced upon the entity's
creation.
