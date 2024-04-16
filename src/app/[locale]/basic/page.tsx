import { notFound } from 'next/navigation'
import { getTranslations } from 'next-intl/server'
import { Container } from 'reactstrap'

import SvgBorder from '@/components/frontend/common/SvgBorder'
import { IS_DEVELOPMENT } from '@/environment'
import Header from '@/layouts/frontend/landing/Header'
import LandingLayout from '@/layouts/frontend/landing/LandingLayout'
import { concatTitle } from '@/utils/helpers'

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('home'))

  return {
    title,
  }
}

export default async function BasicPage({ params: { locale } }: any) {
  const t = await getTranslations({ locale })

  if (!IS_DEVELOPMENT) notFound()

  return (
    <LandingLayout>
      <Header
        title={t('home')}
        description="Create beautiful pages with easy to edit content"
      />
      <section className="bg-white py-10">
        <Container className="px-5">
          <h1 className="pb-3">This is a basic content page.</h1>
          <p className="lead">
            You can use this page as a starting point to create your own custom
            pages, or choose an already built example page to start development!
          </p>
          <p>
            Lorem ipsum dolor sit, amet consectetur adipisicing elit. Qui
            quisquam animi temporibus ipsum iusto necessitatibus laudantium
            beatae. Eligendi dolorum laudantium numquam? Officiis nemo error
            animi aliquam dolor consequatur ducimus unde.
          </p>
          <p>
            Lorem ipsum dolor sit amet consectetur adipisicing elit. Qui
            repellat magni eaque beatae explicabo fugit placeat earum, dolores
            quaerat aperiam vero adipisci quidem minus officiis blanditiis unde?
            Incidunt, ea ad.
          </p>
          <p>
            Lorem ipsum dolor sit amet consectetur adipisicing elit.
            Perspiciatis sed illum soluta, quaerat et deleniti magnam
            laudantium, non omnis numquam quos placeat. Porro autem consectetur
            dolor minima voluptatum modi maiores.
          </p>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  )
}
