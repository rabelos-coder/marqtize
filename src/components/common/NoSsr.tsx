import dynamic from 'next/dynamic'
import { FC, Fragment, ReactNode } from 'react'

type NoSsrProps = {
  children: ReactNode
}

const NoSsr: FC<NoSsrProps> = (props) => <Fragment>{props.children}</Fragment>

export default dynamic(() => Promise.resolve(NoSsr), {
  ssr: false,
})
