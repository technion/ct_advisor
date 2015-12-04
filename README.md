ct_advisor
==========

ct_advisor is a monitoring tool for [Google's Certificate Transparency](https://www.certificate-transparency.org/).

Google offers a number of great options for an administrator to utilise this feature. Unfortunately in a lot of cases, existing infrastructure, particularly if you run Windows servers, makes this difficult.

As an alternative option, this service continually polls the CT log, and will trigger alerts if a certificate is ever registered for your domain, by any CA in the CT program. This can be used to identify fraudulent certificates.

This image this ct_advisor in action:

![CT Advisor Email](https://lolware.net/ct_advisor_email.jpg)

Note that monitors are not instant. Some certificates have taken several days to show up in CT monitor logs.

Monitoring your domain
----------------------

This application is currently running live as a beta status project. If you would like my server to monitor your domain, please email me your request: technion@lolware.net. Please provide all names to monitor and associated email addresses.

Until this tool is marked "stable" it may go offline at any time.

Setup
-----

This application uses a PostgreSQL database, and an SMTP server.

- Install PostgreSQL and create a database
- Run createtables.sql to create tables
- Create priv/credentials.rr in the following format:

```erlang
{database, {credentials, "localhost", "ct_advisor", "password"}}.
{smtp, {credentials, "email-relay.com", "username", "password"}}.
```

Build
-----

This application bundles the tested version of rebar3, and will pull its own external dependancies, of which there are several.

```shell
$ ./rebar3 xref
$ ./rebar3 dialyzer
$ ./rebar3 eunit
$ ./rebar3 ct # TODO
$ /.rebar3 release
```

In development
--------------
It's far easier to utilise my instance of this tool than to attempt to run it yourself - I recommend doing so unless you have a particular need.

The glaring TODO here is a public registration and sign up interface. This has a number of associated problems, such as handling bounces, bots, and considering whether a user actually needs to verify themselves as associated with a domain.

Contributing
------------

* In line with the above, potential contributors should be aware I am unlikely to merge and changes relating to features that I won't be using.
* Code must produce no errors under dialyzer, xref or elvis
* Complex functions must include eunit tests
* Leave your politics at the door

