# Software for Database Interaction in Standard ML

The goal of this project is to allow for database interation in Standard ML. Specifically, the user will input a database and a SQL query, and the result of the SQL query will be returned as a "typed" Standard ML object. By supporting "types" for relations/query outputs, we can ensure that queries are well-typed, and reject ill-typed queries. Our rigorous type-checking system will prevent nonsensical query outputs, which can occur in SQL.

What do we mean by "typed" relations/query outputs? The simplest possible type for a relation is a list of the attributes in that relation. Just introducing this simple type allows for the prevention of problems that can occur in SQL. For example, we can restrict unions to be allowed only when both relations have the same attributes.

Our implementation will include a much more extensive type for relations/query outputs. For each attribute, we will include information such as the column ID, the type of the attribute, whether the attribute can include null values, the default value, any primary/foreign key constraints, and the source table of the attribute in the database. This will allow for rigorous type-checking, and as a result we will be able to prevent many problems by rejecting nonsensical queries that would otherwise be allowed in SQL.

## Journal of Progress

Note: All progress made before November 10, 2019 has been retroactively added to this journal. As a result, the descriptions of progress made in these early dates is not very detailed and slightly unreliable.
