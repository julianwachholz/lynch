/**
 * main.js
 */
/*jslint devel: true */
/*global WebSocket: true, chat: true, message: true */

(function (w, d) {
    'use strict';

    var ws;

    d.addEventListener('DOMContentLoaded', function () {
        ws = new WebSocket('ws://' + w.location.host + '/websocket');

        ws.addEventListener('open', function () {
            console.debug('connected');
        });

        ws.addEventListener('message', function (event) {
            var data = JSON.parse(event.data);
            console.debug('<-', data);

            if (!!data.message) {
                chat.innerHTML += '<' + data.from + '> ' + data.message + '\n';
            }
        });

        ws.addEventListener('close', function () {
            console.debug('disconnected');
        });

        message.addEventListener('keypress', function (event) {
            if (event.charCode === 13) {
                send({message: this.value.trim()});
                this.value = '';
            }
        });

    });

    w.send = function (obj) {
        if (obj instanceof Object) {
            console.debug('->', obj);
            ws.send(JSON.stringify(obj));
        }
    };

}(window, document));
