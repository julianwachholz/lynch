/**
 * main.js
 */
/*jslint devel: true */
/*global WebSocket: true, chat: true, message: true */

(function (w, d) {
    'use strict';

    var WS_URL = 'ws://' + w.location.host + '/websocket';

    /**
     * Simple WS connection with ping.
     */
    var Connection = function (address) {
        this.ws = this.connect(address);
        this.ws.addEventListener('open', this.open.bind(this));
        this.ws.addEventListener('message', this.message.bind(this));
        this.ws.addEventListener('close', this.close.bind(this));
    };

    Connection.TIMEOUT = 5000;
    Connection.PING_INTERVAL = 1000;

    Connection.prototype.connect = function (address) {
        var ws = new WebSocket(address);
        return ws;
    };

    Connection.prototype.open = function() {
        console.debug('WS CONN');
        this.pingTimeout = setTimeout(this.ping.bind(this));
    };

    Connection.prototype.message = function(event) {
        var data = JSON.parse(event.data);

        if (data.type === 'PONG') {
            var ms = performance.now() - data.pong;
            ws_ping.innerHTML = 'Ping: ' + ms + 'ms';
        } else {
            console.debug('WS RECV', data);
        }
    };

    Connection.prototype.send = function (obj) {
        if (obj instanceof Object) {
            if (obj.type !== 'PING') {
                console.debug('WS SEND', obj);
            }
            this.ws.send(JSON.stringify(obj));
        }
    };

    Connection.prototype.close = function() {
        console.debug('WS TERM');
        clearTimeout(this.pingTimeout);
        ws_ping.innerHTML = 'Ping: disconnected';
    };

    Connection.prototype.ping = function() {
        this.send({type: 'PING', ping: performance.now()});
        this.pingTimeout = setTimeout(this.ping.bind(this), Connection.PING_INTERVAL);
    };

    var connection = new Connection(WS_URL);

}(window, document));
