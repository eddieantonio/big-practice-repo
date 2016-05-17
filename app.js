var exampleApp = angular.module('exampleApp', []);

exampleApp
  .controller('CustomerController', function ($scope) {
    $scope.customers = [
      { name: 'Dave', city: 'Phoenix'},
      { name: 'Napur', city: 'Denver' },
      { name: 'Heedy', city: 'Dallas'}
    ];
  });

