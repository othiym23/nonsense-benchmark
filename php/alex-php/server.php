#!/usr/local/bin/php -q
<?php
error_reporting(E_ALL);
set_time_limit(0);
ob_implicit_flush();

$address = 'localhost';
$port = 1337;
$id = 0;
$sock = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);
socket_bind($sock, $address, $port);
socket_listen($sock);

while(true) {
    $msgsock = socket_accept($sock);
    $msg = "ok\n";
    socket_write($msgsock, $msg, strlen($msg));

    while(true) {
        $buf = socket_read($msgsock, 65);

        if (strlen($buf) > 0) {
            $talkback = $buf.':'.work($buf);
            socket_write($msgsock, $talkback, strlen($talkback));
            break;
        }
    }
    socket_close($msgsock);
}

function verify($input, $nonce) {
    $hashed = hash('sha256', $input . $nonce);

    if (substr($hashed, -2) == '00') {
        return true;
    } else {
        return false;
    }
}

function work($input) {
    global $id;

    while (true) {
        $nonce = bin2hex($id);
        $id++;

        if (verify($input, $nonce)) {
            return $nonce;
        }
    }
}
?>