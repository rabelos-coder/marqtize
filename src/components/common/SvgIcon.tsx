import React from 'react'

type SvgIconProps = {
  iconId: string
} & React.SVGProps<SVGSVGElement>

const SvgIcon = ({ iconId, ...res }: SvgIconProps) => {
  return (
    <svg {...res}>
      <use href={`/assets/svg/icon-sprite.svg#${iconId}`}></use>
    </svg>
  )
}

export default SvgIcon
