# Software for Database Interaction in Standard ML

The goal of this project is to allow for database interation in Standard ML. Specifically, the user will input a database and a SQL query, and the result of the SQL query will be returned as a "typed" Standard ML object. By supporting "types" for relations/query outputs, we can ensure that queries are well-typed, and reject ill-typed queries. Our rigorous type-checking system will prevent nonsensical query outputs, which can occur in SQL.

What do we mean by "typed" relations/query outputs? The simplest possible type for a relation is a list of the attributes in that relation. Just introducing this simple type allows for the prevention of problems that can occur in SQL. For example, we can restrict unions to be allowed only when both relations have the same attributes.

Our implementation will include a much more extensive type for relations/query outputs. For each attribute, we will include information such as the column ID, the type of the attribute, whether the attribute can include null values, the default value, any primary/foreign key constraints, and the source table of the attribute in the database. This will allow for rigorous type-checking, and as a result we will be able to prevent many problems by rejecting nonsensical queries that would otherwise be allowed in SQL.

## Journal of Progress

Note: All progress made before November 10, 2019 has been retroactively added to this journal. As a result, the descriptions of progress made in these early dates is not very detailed and slightly unreliable.

Summer 2019: I created a SQLite database called pink.db, which contains multiple relations with information about Pink Floyd. Some examples of relations include "songs", "albums", "people", etc. This database will be very useful for testing purposes once my project begins in the Fall.

10/8/2019: I wrote a very basic SML program to make a SQL query, redirect the output to a file, then read in the file and convert each row to a string (making the result a list of strings, one for each row). Each string represents a tuple in the relation.

10/13/2019: I created a datatype called "queryTerm" which is equal to "Select of string list * string list". This datatype is supposed to represent a "SELECT...FROM" statement, with the first string list being the attributes to select, and the second string list being the relations to select from. I created a function called "getSchema" which takes in a database and a relation, and returns the schema of the relation as a record with an attribute name and attribute type in each entry. I also created scan and parse functions that take in a query and convert to a token list (scan), which is then converted to a queryTerm (parse). Lastly, I slightly modified my function that I created on 10/8/19, so that instead of creating a list of strings, I create a list of lists of strings, where each string corresponds to a specific entry in a tuple.

The ultimate goal is to create a list of records, where each record is a tuple in the relation, and for each entry the label is the attribute name and the value is of the appropriate type (int, text, date, etc.). I plan to accomplish this by using the queryTerm that is created from the parse function to determine which relations to call getSchema on, and then using the queryTerm to determine which attributes to pick out from the getSchema result. Finally, the eval function will take in this modified version of the getSchema output as well as our string list list as inputs, and convert this to our desired list of records.

10/15/2019: I have implemented what was described as my "ultimate goal" in the last entry.

10/21/2019: I changed the internal representation in my program to be in the form of the relational algebra. I changed "queryTerm" to just "term", with the different terms being "Relation of (string * string) list", "CartProd of (term * term)", "NatJoin of (term * term)", and "Proj of (string list * term)". All of these terms evaluate to a Relation term. Now, the evaluation simply produces the schema ("Relation of (string * string) list" can be thought of equivalently to the result from the "getSchema" function). Our final result (the "run" function) takes in a database and a query, and outputs a tuple with two elements: the relation schema from our evaluation, and our original string list list containing the query output. The relation schema can be thought as being the "headers" for the query output. It is interesting to note that since the "WHERE" clause does not affect the schema of the query output, we support this clause by default.

11/3/2019: After a bit of a hiatus, I have returned to work. I updated the representation of the schema from "Relation of (string * string) list" to "Relation of (int * string * string * int * string * int) list". This allows me to include all of the information from calling "PRAGMA table_info(table_name)", rather than just the attribute name and type. I also convert the query output from a string list list to a "SQLtype list list", where the "SQLtypes" are int, text, date, and time (all represented as strings except for int), as well as a recursive "Optional" type to support null values.

11/4/2019: First, I changed the relation to include the name for each entry (for example, instead of just having attr_name I now have ("Attribute Name", attr_name)). This allows for easier readability.

I also resolved a problem that I had thought of a while ago but never got around to nailing down. SQL supports SELECT statements of the form "SELECT attr_name" as well as "SELECT relation_name.attr_name". However, I had a bit of trouble with the latter option because in my evaluation I do joins (natural joins and cartesian products) before projections (selecting specifc attributes). Therefore, when selecting a specific attribute it was unclear which relation the attribute had originally come from. I resolved this by adding another entry to each attribute in my Relation term: ("Tables", *list of source tables*). To understand why this has to be a list of source tables as oppose to just a single table, just think about natural join.

11/9/2019: I added a "noheader" flag to my command line calls to overrule the user's local settings, as not having headers is required according to my implementation. I also changed "Optional" types to not be recursive.

11/10/2019: *todo*
