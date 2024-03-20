'use client'

import { Button, ButtonProps } from 'reactstrap'

import { useRouter } from '@/navigation'

type CustomButtonProps = {
  back?: boolean | undefined
} & ButtonProps

export const CustomButton = ({
  children,
  back,
  onClick,
  ...rest
}: CustomButtonProps) => {
  const router = useRouter()

  const handleClick = (e: any) => {
    if (back) {
      e?.preventDefault()
      router.back()
    } else if (typeof onClick === 'function') {
      e?.preventDefault()
      onClick(e)
    }
  }

  return (
    <Button onClick={handleClick} {...rest}>
      {children}
    </Button>
  )
}
