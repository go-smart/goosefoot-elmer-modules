/**
 * mesher_cgal
 *
 * Copyright (C) 2013-  NUMA Engineering Ltd. (see AUTHORs file)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * This file is adapted from the very permissive CGAL-4.3 examples licence
 * The licence is reproduced as follows:
 *     Copyright (c) 1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007
 *     Utrecht University (The Netherlands),
 *     ETH Zurich (Switzerland),
 *     INRIA Sophia-Antipolis (France),
 *     Max-Planck-Institute Saarbruecken (Germany),
 *     and Tel-Aviv University (Israel).  All rights reserved.
 *
 *     Permission is hereby granted, free of charge, to any person obtaining a
 *     copy of this software and associated documentation files (the
 *     "Software"), to deal in the Software without restriction, including
 *     without limitation the rights to use, copy, modify, merge, publish,
 *     distribute, sublicense, and/or sell copies of the Software, and to
 *     permit persons to whom the Software is furnished to do so, subject to
 *     the following conditions:
 *
 *     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 *     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 *     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 *     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 *     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 *     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 *     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * */

// compute self-intersection of a CGAL triangle polyhedron mesh
// original code from Lutz Kettner
#ifndef _SELF_INTERSECT_H_
#define _SELF_INTERSECT_H_

#include <CGAL/box_intersection_d.h>
#include <CGAL/intersections.h>
#include <CGAL/Bbox_3.h>

template <class Polyhedron, class Kernel, class OutputIterator>
struct Intersect_facets
{
  typedef typename Kernel::Point_3      Point;
  typedef typename Kernel::Vector_3     Vector;
  typedef typename Kernel::Segment_3    Segment;
  typedef typename Kernel::Triangle_3   Triangle;
  typedef typename Polyhedron::Facet_const_handle       Facet_const_handle;
  typedef typename Polyhedron::Facet_const_iterator     Facet_const_iterator;
  typedef typename Polyhedron::Halfedge_const_handle    Halfedge_const_handle;
  typedef typename CGAL::Box_intersection_d::Box_with_handle_d<double, 3, Facet_const_handle> Box;
  mutable OutputIterator m_iterator;

public:

  Intersect_facets(OutputIterator it)
    : m_iterator(it)
  {
  }

  void operator()(const Box* b,
    const Box* c) const
  {
    Halfedge_const_handle h = b->handle()->halfedge();

    // check for shared egde --> no intersection
    if(h->opposite()->facet() == c->handle() ||
      h->next()->opposite()->facet() == c->handle() ||
      h->next()->next()->opposite()->facet() == c->handle())
      return;

    // check for shared vertex --> maybe intersection, maybe not
    Halfedge_const_handle g = c->handle()->halfedge();
    Halfedge_const_handle v;

    if(h->vertex() == g->vertex())
      v = g;
    if(h->vertex() == g->next()->vertex())
      v = g->next();
    if(h->vertex() == g->next()->next()->vertex())
      v = g->next()->next();

    if(v == Halfedge_const_handle())
    {
      h = h->next();
      if(h->vertex() == g->vertex())
	v = g;
      if(h->vertex() == g->next()->vertex())
	v = g->next();
      if(h->vertex() == g->next()->next()->vertex())
	v = g->next()->next();
      if(v == Halfedge_const_handle())
      {
	h = h->next();
	if(h->vertex() == g->vertex())
	  v = g;
	if(h->vertex() == g->next()->vertex())
	  v = g->next();
	if(h->vertex() == g->next()->next()->vertex())
	  v = g->next()->next();
      }
    }

    if(v != Halfedge_const_handle())
    {
      // found shared vertex:
      CGAL_assertion(h->vertex() == v->vertex());
      // geometric check if the opposite segments intersect the triangles
      Triangle t1( h->vertex()->point(),
	h->next()->vertex()->point(),
	h->next()->next()->vertex()->point());
      Triangle t2( v->vertex()->point(),
	v->next()->vertex()->point(),
	v->next()->next()->vertex()->point());
      Segment s1( h->next()->vertex()->point(),
	h->next()->next()->vertex()->point());
      Segment s2( v->next()->vertex()->point(),
	v->next()->next()->vertex()->point());

      if(CGAL::do_intersect(t1,s2))
      {
	*m_iterator++ = t1;
	*m_iterator++ = t2;
      }
      else
	if(CGAL::do_intersect(t2,s1))
	{
	  *m_iterator++ = t1;
	  *m_iterator++ = t2;
	}
	return;
    }

    // check for geometric intersection
    Triangle t1( h->vertex()->point(),
      h->next()->vertex()->point(),
      h->next()->next()->vertex()->point());
    Triangle t2( g->vertex()->point(),
      g->next()->vertex()->point(),
      g->next()->next()->vertex()->point());
    if(CGAL::do_intersect(t1, t2))
    {
      *m_iterator++ = t1;
      *m_iterator++ = t2;
    }
  } // end operator ()
}; // end struct Intersect_facets

template <class Polyhedron, class Kernel, class OutputIterator>
void self_intersect(const Polyhedron& polyhedron,
		    OutputIterator out)
{
  typedef CGAL::Bbox_3                  Bbox; // always double
  typedef typename Kernel::Point_3      Point;
  typedef typename Kernel::Vector_3     Vector;
  typedef typename Kernel::Triangle_3   Triangle;
  typedef typename Kernel::Segment_3    Segment;
  typedef typename Polyhedron::Halfedge_const_handle    Halfedge_const_handle;
  typedef typename Polyhedron::Facet_const_iterator     Facet_const_iterator;
  typedef typename Polyhedron::Facet_const_handle       Facet_const_handle;
  typedef typename CGAL::Box_intersection_d::Box_with_handle_d<double, 3, Facet_const_handle> Box;

  // make one box per facet
  std::vector<Box> boxes;
  boxes.reserve(polyhedron.size_of_facets());

  Facet_const_iterator f;
  for(f = polyhedron.facets_begin();
    f != polyhedron.facets_end();
    f++)
    boxes.push_back(Box( f->halfedge()->vertex()->point().bbox() +
    f->halfedge()->next()->vertex()->point().bbox() +
    f->halfedge()->next()->next()->vertex()->point().bbox(),
    f));

  // generate box pointers
  std::vector<const Box*> box_ptr;
  box_ptr.reserve(polyhedron.size_of_facets());
  typename std::vector<Box>::iterator b;
  for(b = boxes.begin();
    b != boxes.end();
    b++)
    box_ptr.push_back(&*b);

  // compute self-intersections filtered out by boxes
  Intersect_facets<Polyhedron,Kernel,OutputIterator> intersect_facets(out);
  std::ptrdiff_t cutoff = 2000;
  CGAL::box_self_intersection_d(box_ptr.begin(), box_ptr.end(),intersect_facets,cutoff);

} // end self_intersect

#endif // _SELF_INTERSECT_H_
