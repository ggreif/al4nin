/*
 * Copyright (c) 2004 Gabor Greif
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is furnished to do
 * so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
 * OR OTHER DEALINGS IN THE SOFTWARE.
 */

#ifndef _ALLOC_A4L_
#define _ALLOC_A4L_

#include <memory>

namespace aL4nin
{
    template <typename T>
    struct meta;

    template <typename T>
    meta<T>& get_meta(std::size_t);

    template <typename T>
    struct alloc : std::allocator<T>
    {
        typedef typename std::allocator<T>::pointer pointer;

        template <typename U>
        struct rebind
        {
            typedef alloc<U> other;
        };

        pointer allocate(std::size_t);
        void deallocate(pointer, size_t);

        alloc(void) throw() { }

        alloc(const alloc& a) throw()
            : std::allocator<T>(a) { }

        template<typename U>
        alloc(const alloc<U>&) throw() { }

        ~alloc(void) throw() { }
    };
}


#endif // _ALLOC_A4L_
