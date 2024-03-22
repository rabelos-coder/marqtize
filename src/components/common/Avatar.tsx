import Image from 'next/image'

import { generateBackground, getInitials } from '@/utils/helpers'

export type AvatarProps = {
  name: string
  image?: string
  className?: string
  size?: number
}

export function Avatar({ name, size, image, className }: AvatarProps) {
  const initials = getInitials(name)
  const color = generateBackground(name)
  const style = {
    display: 'flex',
    height: size ? `${size}px` : '32px',
    width: size ? `${size}px` : '32px',
    borderRadius: '100px',
    color: 'white',
    backgroundColor: color as any,
    margin: 'auto',
  }

  return image ? (
    <Image
      src={`${image}`}
      alt={name}
      width={size ?? 32}
      height={size ?? 32}
      style={style}
      className={`img-fluid table-avatar ${className}`}
    />
  ) : (
    <div style={style} className="table-avatar">
      <span style={{ margin: 'auto' }} className={`${className}`}>
        {initials}
      </span>
    </div>
  )
}
