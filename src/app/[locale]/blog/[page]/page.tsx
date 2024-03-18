import { capitalize } from "lodash";
import Image from "next/image";
import { notFound } from "next/navigation";
import { getTranslations } from "next-intl/server";
import { Card, CardBody, CardFooter, Col, Container, Row } from "reactstrap";

import { SvgBorder } from "@/components/frontend/common/SvgBorder";
import { PAGINATED_POSTS } from "@/graphql/blogPost";
import { Header } from "@/layout/frontend/landing/Header";
import { LandingLayout } from "@/layout/frontend/landing/LandingLayout";
import { Pagination } from "@/layout/frontend/landing/Pagination";
import { Link } from "@/navigation";
import { createApolloClient } from "@/utils/apollo";
import { DateTime } from "@/utils/date";
import { concatTitle } from "@/utils/helpers";

const client = createApolloClient();

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale });
  const title = concatTitle(t("blog.title"));

  return {
    title,
  };
}

export default async function PostsPage({ params: { locale, page } }: any) {
  const t = await getTranslations({ locale });

  page = parseInt(`${page}`);

  const { data, error } = await client.query({
    query: PAGINATED_POSTS,
    variables: {
      page,
      perPage: 6,
      where: {
        AND: [
          {
            publishedAt: {
              lte: new Date().toISOString(),
            },
          },
        ],
        isPublished: true,
        deletedAt: null,
      },
      orderBy: {
        publishedAt: "desc",
      },
    },
  });

  if (error) throw new Error(error?.message);

  const { paginatedBlogPost } = data;

  if (
    page <= 0 ||
    (paginatedBlogPost?.meta?.lastPage &&
      paginatedBlogPost.meta.lastPage < page)
  ) {
    notFound();
  }

  return (
    <LandingLayout>
      <Header
        title={t("blog.title")}
        description={t("blog.shortDescription")}
      />
      <section className="bg-light py-10">
        <Container className="px-5">
          <Row className="gx-5 blog-posts">
            {paginatedBlogPost?.meta?.total === 0 ? (
              <Col lg={8}>
                <div className="alert alert-warning">{t("blog.noPosts")}</div>
              </Col>
            ) : (
              paginatedBlogPost?.data?.map((post) => {
                return (
                  <Col key={post.id} md={6} xl={4} className="mb-5">
                    <Card
                      className="post-preview post-preview-featured lift mb-5 overflow-hidden"
                      tag={Link}
                      href={`/blog/post/${post.slug}`}
                    >
                      {post.coverImage && (
                        <Image
                          src={post.coverImage}
                          width={660}
                          height={360}
                          className="card-img-top"
                          alt={post.title}
                        />
                      )}
                      <CardBody>
                        <h5 className="card-title">{capitalize(post.title)}</h5>
                        <p className="card-text text-justify">{post.resume}</p>
                      </CardBody>
                      <CardFooter>
                        <div className="post-preview-meta">
                          <Image
                            width={300}
                            height={300}
                            className="post-preview-meta-img"
                            src={
                              post.author?.image ??
                              "/assets/images/theme/landing/profiles/profile-2.png"
                            }
                            alt={post.author?.name ?? "Avatar"}
                          />
                          <div className="post-preview-meta-details">
                            {post.author?.name && (
                              <div className="post-preview-meta-details-name">
                                {post.author.name}
                              </div>
                            )}
                            <div className="post-preview-meta-details-date">
                              {new DateTime(post.publishedAt)
                                .locale(locale)
                                .format()}
                            </div>
                          </div>
                        </div>
                      </CardFooter>
                    </Card>
                  </Col>
                );
              })
            )}
          </Row>
          <Pagination meta={paginatedBlogPost?.meta} />
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  );
}
