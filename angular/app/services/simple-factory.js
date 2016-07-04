app.factory('simpleFactory', function () {
  var customers = [
    { name: 'Dave', city: 'Phoenix'},
    { name: 'Napur', city: 'Denver' },
    { name: 'Heedy', city: 'Dallas'}
  ];

  return {
    getCustomers() {
      return customers;
    }
  };
});
