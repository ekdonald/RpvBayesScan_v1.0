## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
## 02111-1307, USA.
##
## $Id$

include $(top_srcdir)/Makefile.am.common

# the library names to build (note we are building static libs only)
lib_LIBRARIES = libmultinest.a

# the list of header files that belong to the library (to be installed later)
#libdarksusy_a_Headers =  ../inc/dsaccom.h

libmultinest_a_SOURCES =  \
	utils.f90 utils1.f90 priors.f90 kmeans_clstr.f90 xmeans_clstr.f90 posterior.F90 nested.F90

## Remove generated files
clean-local:
	rm -f *.mod
