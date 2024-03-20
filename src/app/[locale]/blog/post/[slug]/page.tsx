import { capitalize } from "lodash";
import { Metadata } from "next";
import { headers } from "next/headers";
import Image from "next/image";
import { notFound } from "next/navigation";
import { getTranslations } from "next-intl/server";
import { Fragment } from "react";
import { Col, Container, Row } from "reactstrap";

import { CustomButton } from "@/components/common/CustomButton";
import { ShareButtons } from "@/components/common/ShareButtons";
import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import {
  APP_META_DESCRIPTION,
  APP_META_TITLE,
  APP_WEBSITE,
} from "@/environment";
import { FIND_FIRST_POST } from "@/graphql/blogPost";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { Link } from "@/navigation";
import { createApolloClient } from "@/utils/apollo";
import { DateTime } from "@/utils/date";
import { concatTitle } from "@/utils/helpers";

const client = createApolloClient();

export async function generateMetadata({
  params: { locale, slug },
}: any): Promise<Metadata> {
  const t = await getTranslations({ locale });
  let title = concatTitle(t("blog.title"));
  const headersList = headers();

  const referer = headersList?.get("referer") ?? APP_WEBSITE;
  const refererURL = new URL(referer);
  const protocol = refererURL.protocol;

  const host = headersList?.get("host") ?? "";
  const baseUrl = `${protocol}//${host}`;
  const url = headersList?.get("next-url")
    ? `${baseUrl}${headersList?.get("next-url")}`
    : "http://localhost:3000";

  const { data, error } = await client.query({
    query: FIND_FIRST_POST,
    variables: {
      where: {
        AND: [
          {
            publishedAt: {
              lte: new Date().toISOString(),
            },
          },
        ],
        slug,
        deletedAt: null,
      },
    },
  });

  if (error) console.log(error?.message);

  const { findFirstBlogPost } = data;
  if (findFirstBlogPost)
    title = concatTitle(capitalize(findFirstBlogPost.title));

  return {
    title,
    metadataBase: new URL(baseUrl),
    alternates: {
      canonical: "/",
      languages: {
        en: "/en",
        "en-US": "/en",
        pt: "/pt-br",
        "pt-BR": "/pt-br",
      },
    },
    openGraph: {
      title,
      locale,
      type: "article",
      description: findFirstBlogPost?.resume ?? APP_META_DESCRIPTION,
      url,
      siteName: APP_META_TITLE,
      images: [
        {
          url: "/assets/images/themes/landing/og-image-800x600.png",
          width: 800,
          height: 600,
        },
        {
          url: "/assets/images/themes/landing/og-image-1800x1600.png",
          width: 1800,
          height: 1600,
        },
      ],
    },
  };
}

export default async function BlogPostPage({ params: { locale, slug } }: any) {
  const t = await getTranslations({ locale });

  let title = t("blog.categories.title");

  const { data, error } = await client.query({
    query: FIND_FIRST_POST,
    variables: {
      where: {
        AND: [
          {
            publishedAt: {
              lte: new Date().toISOString(),
            },
          },
        ],
        slug,
        deletedAt: null,
      },
    },
  });

  if (error) throw new Error(error?.message);

  const { findFirstBlogPost } = data;
  if (findFirstBlogPost) {
    title = capitalize(findFirstBlogPost.title);
  } else {
    notFound();
  }

  return (
    <LandingLayout navbarExpanded>
      <section className="bg-light py-10">
        <Container className="px-5">
          <Row className="gx-5 justify-content-center">
            <Col lg={10} xl={8}>
              <div className="single-post">
                <h1 className="mb-4">{title}</h1>
                <p className="lead text-justify mb-5">
                  {findFirstBlogPost.resume}
                </p>
                <div className="d-flex align-items-center justify-content-between mb-1 pb-3">
                  <div className="single-post-meta me-4">
                    <Image
                      width={300}
                      height={300}
                      className="single-post-meta-img"
                      src={
                        findFirstBlogPost.author?.image ??
                        "/assets/images/user/user.jpg"
                      }
                      alt={findFirstBlogPost.author?.name ?? "Avatar"}
                    />
                    <div className="single-post-meta-details">
                      {findFirstBlogPost.author?.name && (
                        <div className="single-post-meta-details-name">
                          {findFirstBlogPost.author.name}
                        </div>
                      )}
                      <div className="single-post-meta-details-date">
                        {new DateTime(findFirstBlogPost.publishedAt)
                          .locale(locale)
                          .format()}
                      </div>
                    </div>
                  </div>
                  <ShareButtons />
                </div>
                <hr className="my-0" />
                <Row className="gx-5 mt-3 mb-0 post-meta-data">
                  <Col md={6} sm={12} className="post-meta-categories">
                    <h6>{t("blog.categories.title")}</h6>
                    {findFirstBlogPost.categories?.map((category, i) => (
                      <Fragment key={category.id}>
                        <Link href={`/blog/category/${category.slug}/1`}>
                          {category.name}
                        </Link>
                        {i <
                          (findFirstBlogPost.categories?.length ?? 1) - 1 && (
                          <>,&nbsp;</>
                        )}
                      </Fragment>
                    ))}
                  </Col>
                  <Col
                    md={6}
                    sm={12}
                    className="post-preview-meta-tags text-xl-end"
                  >
                    <h6>{t("blog.tags.title")}</h6>
                    {findFirstBlogPost.tags?.map((tag, i) => (
                      <Fragment key={tag.id}>
                        <Link key={tag.id} href={`/blog/tag/${tag.slug}/1`}>
                          {tag.name}
                        </Link>
                        {i < (findFirstBlogPost.tags?.length ?? 1) - 1 && (
                          <>,&nbsp;</>
                        )}
                      </Fragment>
                    ))}
                  </Col>
                </Row>
                {findFirstBlogPost.coverImage && (
                  <Image
                    className="img-fluid mb-2 rounded"
                    width={695}
                    height={380}
                    src={findFirstBlogPost.coverImage}
                    alt={findFirstBlogPost.title}
                  />
                )}
                <div
                  className="single-post-text my-5 text-justify"
                  dangerouslySetInnerHTML={{
                    __html: findFirstBlogPost.content,
                  }}
                ></div>
                <hr className="my-5" />
                <div className="text-center">
                  <CustomButton className="btn-transparent-dark" back>
                    {t("backToBlog")}
                  </CustomButton>
                </div>
              </div>
            </Col>
          </Row>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
