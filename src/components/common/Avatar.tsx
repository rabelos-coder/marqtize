'use client'

import Image from 'next/image'
import { v4 as uuidv4 } from 'uuid'

export type AvatarProps = {
  name: string
  id?: number
  size?: number
  image?: string | null
  rounded?: boolean
  className?: string
}

/**
 * Generates an avatar component based on the provided name, size, image, and className.
 *
 * @param {AvatarProps} props - The props to be used in the avatar.
 * @return {JSX.Element} The avatar component to be rendered.
 */
export default function Avatar({
  id,
  name,
  size,
  image,
  rounded,
  className,
}: AvatarProps): JSX.Element {
  const defaultSize = 50
  const style: any = {
    height: size ? `${size}px` : `${defaultSize}px`,
    width: size ? `${size}px` : `${defaultSize}px`,
    borderRadius: '100px',
  }

  if (!rounded) delete style.borderRadius

  const imageUrl = decodeURIComponent(
    `/api/image/${size ?? defaultSize}/${name.toUpperCase()}`
  )

  return image ? (
    <img
      id={id ?? uuidv4()}
      src={`${image}`}
      alt={name}
      width={size ?? defaultSize}
      height={size ?? defaultSize}
      style={style}
      className={`${className}`}
    />
  ) : (
    <Image
      id={id ?? uuidv4()}
      src={imageUrl}
      alt={name}
      width={size ?? defaultSize}
      height={size ?? defaultSize}
      style={style}
      className={`${className}`}
    />
  )
}
