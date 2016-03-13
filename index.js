const feathers = require('feathers');
const memory = require('feathers-memory');
const bodyParser = require('body-parser');
const users = require('./fixtures/users.json');
const chatService = memory();
const participantsService = memory();
const usersService = memory();

Object.keys(users).forEach(handle => {
  usersService.create(users[handle]);
});

const app = feathers()
  .configure(feathers.rest())
  .configure(feathers.socketio(io => {
    io.on('connection', socket => {
      socket.on('statusChange', participant => {
        participantsService.create(participant)
      });
    });
  }))
  .use(bodyParser.json())
  .use(bodyParser.urlencoded({ extended: true }))
  .use(feathers.static(__dirname))
  .use('/chat', chatService)
  .use('/users', usersService)
  .use('/participants', participantsService);

app.listen(8080);
