'use client'

import Image from 'next/image'

export type AvatarProps = {
  name: string
  size?: number
  image?: string
  className?: string
}

/**
 * Generates an avatar component based on the provided name, size, image, and className.
 *
 * @param {AvatarProps} props - The props to be used in the avatar.
 * @return {JSX.Element} The avatar component to be rendered.
 */
export function Avatar({
  name,
  size,
  image,
  className,
}: AvatarProps): JSX.Element {
  const defaultSize = 50
  const style = {
    height: size ? `${size}px` : `${defaultSize}px`,
    width: size ? `${size}px` : `${defaultSize}px`,
    borderRadius: '100px',
  }

  const imageUrl = decodeURIComponent(
    `/api/image/${size ?? defaultSize}/${name}`
  )

  return image ? (
    <Image
      src={`${image}`}
      alt={name}
      width={size ?? defaultSize}
      height={size ?? defaultSize}
      style={style}
      className={`img-fluid table-avatar ${className}`}
    />
  ) : (
    <Image
      src={imageUrl}
      alt={name}
      width={size ?? defaultSize}
      height={size ?? defaultSize}
      style={style}
      className={`img-fluid table-avatar ${className}`}
    />
  )
}
