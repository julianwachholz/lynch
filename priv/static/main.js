/**
 * main.js
 */
(function (w, d) {
    'use strict';

    var ws;

    d.addEventListener('DOMContentLoaded', function () {
        ws = new WebSocket('ws://' + window.location.host + '/websocket');

        ws.addEventListener('open', function () {
            console.debug('WS connected.');
        });

        ws.addEventListener('message', function (event) {
            console.debug('WS message:', event.data);
        });

        ws.addEventListener('close', function () {
            console.debug('WS disconnected.');
        });
    });

    window.send = function (msg) {
        ws.send(JSON.stringify(msg));
    };

}(window, document));
