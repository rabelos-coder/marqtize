'use client'

type SpinnerLineProps = {
  type?: 'border' | 'grow'
  className?: string
  color?:
    | 'primary'
    | 'secondary'
    | 'success'
    | 'danger'
    | 'warning'
    | 'info'
    | 'light'
    | 'dark'
    | 'default'
}

export const SpinnerLine = ({ type, color, className }: SpinnerLineProps) => {
  return (
    <div
      className={`d-flex justify-content-center align-items-center ${className}`}
    >
      <div
        className={`spinner-${type ?? 'border'} text-${color ?? 'default'}`}
        role="status"
      ></div>
    </div>
  )
}
