ct_advisor
=====

ct_advisor is a monitoring tool for [Google's Certificate Transparency](https://www.certificate-transparency.org/).

Google offers a number of great options for an administrator to utilise this feature. Unfortunately in a lot of cases, existing infrastructure, particularly if you run Windows servers, makes this difficult.

As an alternative option, this service continually polls the CT log, and will trigger alerts if a certificate is ever registered for your domain, by any CA in the CT program. This can be used to identify fraudulent certificates.

Build
-----

    $ ./rebar3 compile
    $ /.rebar3 shell

Database
--------

This application uses a PostgreSQL database.

- Install PostgreSQL and create a database
- Run createtables.sql
- Create priv/credentials.rr in the following format:

    {credentials, "localhost", "ct_advisor", "PASSWORDHERE"}.

In development
--------------
More documentation to be written as the project takes shape. Current status:
- [x] Communicate with CT REST API
- [x] Parse the overly complicated Merkle Tree Leaf format
- [x] Parse X509 certificates for DNS names
- [x] Run as a scheduled service
- [x] Match domains with sign up list
- [x] Dialyzer hints on all functions
- [x] Eunit Test suite
- [x] Logging framework
- [x] Persist last lookup state across restarts
- [ ] Code audit, cleanup, review
- [ ] Actually email out alerts
- [ ] Management interface for user/domain listing
- [ ] Common Test
- [ ] Document public access
