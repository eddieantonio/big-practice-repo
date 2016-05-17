var app = angular.module('exampleApp', ['ngRoute']);

app.value('appName', 'Example App');

app.config(function ($routeProvider) {
  $routeProvider
    .when('/', {
      controller: 'CustomerController',
      templateUrl: 'app/views/index.html'
    })
    .when('/alt', {
      controller: 'CustomerController',
      templateUrl: 'app/views/alternate.html'
    })
    .otherwise({ redirectTo: '/' });
});
