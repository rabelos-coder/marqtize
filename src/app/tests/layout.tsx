import '../../assets/scss/globals.scss'

import { ChildrenProps } from '@/types/common'

export default function TestLayout({ children }: ChildrenProps) {
  return (
    <html lang="pt-br">
      <body>{children}</body>
    </html>
  )
}
