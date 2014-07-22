/**
 * main.js
 */
/*jslint devel: true */
/*global WebSocket: true, chat: true, message: true */

(function (window, d) {
    'use strict';

    var ws;

    d.addEventListener('DOMContentLoaded', function () {
        ws = new WebSocket('ws://' + window.location.host + '/websocket');

        ws.addEventListener('open', function () {
            console.debug('WS connected.');
        });

        ws.addEventListener('message', function (event) {
            var data = JSON.parse(event.data);
            if (!!data.message) {
                chat.innerHTML += '<' + data.from + '> ' + data.message + '\n';
            }
        });

        ws.addEventListener('close', function () {
            console.debug('WS disconnected.');
        });

        message.addEventListener('keypress', function (event) {
            if (event.charCode === 13) {
                ws.send(JSON.stringify({message: this.value.trim()}));
                this.value = '';
            }
        });

    });

}(window, document));
