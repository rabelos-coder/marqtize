import { CSSProperties } from 'react'

import { generateBackground, getInitials } from '@/utils/helpers'

type LetteredAvatarProps = {
  name: string
  size?: number
  rounded?: boolean
}

export function LetteredAvatar({ name, size, rounded }: LetteredAvatarProps) {
  const initials = getInitials(name)
  const color = generateBackground(name)

  const defaultSize = 50
  const sizeNumber = size ? size : defaultSize
  const sizeFormatted = `${sizeNumber}px`
  const fontSizeNumber = sizeNumber / 2.5
  const fontSizeFormatted = `${fontSizeNumber}px`

  const customStyle: CSSProperties = {
    display: 'flex',
    height: sizeFormatted,
    width: sizeFormatted,
    color: '#ffffff',
    background: `#${color}`,
    fontSize: fontSizeFormatted,
    margin: 'auto',
  }

  if (rounded) {
    customStyle['borderRadius'] = '100px'
  }

  return (
    <div style={customStyle}>
      <span style={{ margin: 'auto' }}> {initials} </span>
    </div>
  )
}
