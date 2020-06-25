kittychat
=====

An OTP application

Build
-----

    $ rebar3 compile
    
    
/login?name=user1
returns opaque ID

/messages-for-me?id=opaque
all messages

/who
/message?id=opaque&recipient=harry&msg=hello
/wall?msg="hello everybody"
