import { Roboto } from 'next/font/google'
import { Container } from 'reactstrap'

const roboto = Roboto({
  subsets: ['latin'],
  weight: ['500', '700'],
  display: 'swap',
  preload: true,
})

export default function SubDomainPage({ params: { locale, subdomain } }: any) {
  return (
    <Container>
      <h1 className={` fw-bold ${roboto.className}`}>
        Page Sub-domain {subdomain} em {locale}
      </h1>
    </Container>
  )
}
