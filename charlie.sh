#!/bin/bash


    echo "Charlie logs in"
    Charlie_ID=`curl -s localhost:8080/login?name=charlie`
    echo "URL-encoded ID for charlie is $Charlie_ID"
    echo
    
    echo "Lucy logs in"
    Lucy_ID=`curl -s localhost:8080/login?name=lucy`
    echo "URL-encoded ID for lucy is $Charlie_ID"
    echo
    
    echo "Retrieving messages for Charlie"
    Reply=`curl -s localhost:8080/messages-for-me?id=$Charlie_ID`
    echo "Server reply: $Reply"
    echo
    
    echo "Retrieving messages for Lucy"
    Reply=`curl -s localhost:8080/messages-for-me?id=$Lucy_ID`
    echo "Server reply: $Reply"
    echo
    
    echo "Charlie sends message to Lucy"
    Reply=`curl -s "localhost:8080/message?id=$Charlie_ID&recipient=lucy&message=hello-lucy"`
    echo "Server reply: $Reply"
    echo
    
    echo "Charlie sends another message to Lucy"
    Reply=`curl -s "localhost:8080/message?id=$Charlie_ID&recipient=lucy&message=are-you-there"`
    echo "Server reply: $Reply"
    echo

    echo "Lucy gets her messages"
    Reply=`curl -s localhost:8080/messages-for-me?id=$Lucy_ID`
    echo "Server reply: $Reply"
    echo
