import { cookies } from 'next/headers'
import { getTranslations } from 'next-intl/server'
import { Container, Row } from 'reactstrap'

import Breadcrumbs from '@/components/backend/Breadcrumbs'
import AclGuard from '@/components/backend/Guards/AclGuard'
import { STORAGE_AUTH_TOKEN } from '@/configs'
import { WHO_AM_I } from '@/graphql/auth'
import { FIND_MANY_TIMEZONES } from '@/graphql/localization'
import { PageParamsProps } from '@/types/common'
import { createApolloClient } from '@/utils/apollo'
import { concatTitle } from '@/utils/helpers'
import EditMyProfile from '@/views/backend/system/profile/EditMyProfile'
import EditProfileForm from '@/views/backend/system/profile/EditProfileForm'

export async function generateMetadata({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('myProfile'))

  return {
    title,
  }
}

export const dynamic = 'force-dynamic'

export default async function ProfilePage({
  params: { locale },
}: PageParamsProps) {
  const t = await getTranslations({ locale })

  const cookieStore = cookies()
  const token = cookieStore.get(STORAGE_AUTH_TOKEN)

  const client = createApolloClient({ token: token?.value, locale })

  const { data } = await client.query({
    query: WHO_AM_I,
    fetchPolicy: 'no-cache',
  })

  const { data: timezonesData } = await client.query({
    query: FIND_MANY_TIMEZONES,
  })

  const languages: Array<{ label: string; value: string }> = [
    // {
    //   label: t("english"),
    //   value: "en",
    // },
    {
      label: t('portuguese'),
      value: 'pt-br',
    },
  ]

  const timezones: Array<{ label: string; value: string }> =
    (timezonesData?.findManyTimezone?.map((timezone) => ({
      label: timezone.name,
      value: timezone.id,
    })) as any[]) || []

  return (
    <AclGuard>
      <div className="page-body">
        <Breadcrumbs title={t('profile')} pageTitle={t('profile')} />
        <Container fluid>
          <div className="edit-profile">
            <Row>
              <EditMyProfile />
              <EditProfileForm
                user={data.whoAmI}
                timezones={timezones}
                languages={languages}
              />
            </Row>
          </div>
        </Container>
      </div>
    </AclGuard>
  )
}
