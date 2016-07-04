#!/usr/bin/env ruby

# Copyright (C) 2016 Eddie Antonio Santos <easantos@ualberta.ca>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

require './local-weather'

weather = LocalWeather.fetch

puts "The condition is #{weather.condition}"

case
when weather.snowy? && weather.really_cold?
  puts "It's snowy and cold :C"
when weather.really_cold?
  puts "Wear a warm coat!"
  puts "It's #{weather.temperature} degrees for crying out loud!"
when weather.cold?
  puts "Wear a sweater."
end
