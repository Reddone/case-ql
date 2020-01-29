# Case QL

Small library providing basic CRUD operations for SQL backend when using GraphQL. At the moment it only supports 
[doobie](https://github.com/tpolecat/doobie) for SQL and [sangria](https://github.com/sangria-graphql/sangria) 
for GraphQL, but I am planning to add support for [scalikejdbc](https://github.com/scalikejdbc/scalikejdbc) and 
[caliban](https://github.com/ghostdogpr/caliban). I am not planning to add [slick](https://github.com/slick/slick) and 
[quill](https://github.com/getquill/quill) support because they are not based or they not provide support for the 
fragment approach (I strongly believe that we should write SQL and not try to port SQL inside Scala).

It leverages scala case classes and shapeless to build type safe SQL queries from a GraphQL Input. The project was 
born because I was tired of writing boilerplate for SQL, especially complex filters. Since GraphQL queries are 
"client-driven" the most time consuming task was to write all the kind of possible filters for entities; this problem 
was also addressed by [prisma](https://www.prisma.io/docs/reference/prisma-api/queries-ahwee4zaey#filtering-by-field), 
but honestly even if the project is very interesting I'd like to stick with Scala and custom code, especially if some 
queries require SQL ad-hoc customization. Actually there is no support for relations, I am open to discussions on how 
we can add them.

See the "example" sub project to see it in action! It is still an experiment but it helped me a lot!!!

## Introduction

I am writing examples along with tests and documentation these month, so please
wait a little. If you want you can explore the source code, the core is located
inside the "generic" package of the "sql" module.

## Table

WORK IN PROGRESS

<!-- [here](./docs/table.md) -->

## Filter

WORK IN PROGRESS

<!-- [here](./docs/filter.md) -->

## Modifier

WORK IN PROGRESS

<!-- [here](./docs/modifier.md) -->

## Query

WORK IN PROGRESS

<!-- [here](./docs/modifier.md) -->

## Util

WORK IN PROGRESS

<!-- [here](./docs/util.md) -->

## TODO

- setup an easy build and release process and move to sonatype
- add a combinator option for both the entire filter and each singular field (actually it defaults to AND) 
- abstract over Fragment in order to include scalalikejdbc support
- provide support for caliban (I still have to study the project)
- add relations (one to one, one to many, many to many)
- more examples
- fix spacing inside queries (not necessary but nice to have)
- add a logo if I get 10 stars
