'use client'
import { useEffect, useRef } from 'react'

type ComponentProps = {
  className?: string
  src: string
  alt: string
}

const RatioImage = (props: ComponentProps) => {
  const { alt } = props
  const bgImg = useRef<HTMLImageElement>(null)

  useEffect(() => {
    const image = bgImg.current
    if (image && image.classList.contains('bg-img-cover')) {
      const parentElement = image.parentElement
      const src = image.getAttribute('src')
      if (parentElement) {
        parentElement.classList.add('bg-size')
        image.style.display = 'none'
        parentElement.setAttribute(
          'style',
          `
          background-image: url(${src});
          background-size: cover;
          background-position: center;
          background-repeat: no-repeat;
          display: block;
          `
        )
      }
    }
  }, [])

  return <img ref={bgImg} {...props} alt={alt} />
}
export default RatioImage
