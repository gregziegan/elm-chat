const feathers = require('feathers');
const memory = require('feathers-memory');
const bodyParser = require('body-parser');
const users = require('./fixtures/users.json');
const chatService = memory();
const usersService = memory();

const users_typing = {};
Object.keys(users)
  .map(handle => users[handle])
  .forEach(user => {
    users_typing[user.id] = (user, false);
    usersService.create(user);
  });

const app = feathers()
  .configure(feathers.rest())
  .configure(feathers.socketio(function (io) {
    io.on('connection', function (socket) {
      socket.on('typing', function (userTyping) {
        const user = userTyping[0];
        const isTyping = userTyping[1];
        users_typing[user.id] = userTyping;
        const whoIsTyping = Object.keys(users_typing)
          .map(k => users_typing[k])
          .filter(userTyping => userTyping[1])
          .map(userTyping => userTyping[0]);
        io.emit('typing', whoIsTyping);
      });
    });
  }))
  .use(bodyParser.json())
  .use(bodyParser.urlencoded({ extended: true }))
  .use(feathers.static(__dirname))
  .use('/chat', chatService)
  .use('/users', usersService);

app.listen(8080);
