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

require 'set'

class String
  def shingles(size=2)
    ω = size
    return Set[self] if length <= ω

    Set.new (ω..length).each do |i|
      self[i - ω...i]
    end
  end

  def simhash
    v = [0] * 32
    hashes = shingles.map { |s| s.hash }
    hashes.each do |hash|
      v.each_index do |i|
        if hash[i].zero?
          v[i] -= 1
        else
          v[i] += 1
        end
      end
    end

    result = 0

    v.each_with_index do |value, i|
      result |= (1 << i) if value > 0
    end

    result
  end
end
