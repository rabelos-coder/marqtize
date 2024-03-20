"use client";

import { capitalize } from "lodash";
import Image from "next/image";
import { useLocale, useTranslations } from "next-intl";
import { Fragment } from "react";
import { Card, CardBody, CardFooter, Col, Container, Row } from "reactstrap";

import { Pagination } from "@/layout/frontend/landing/Pagination";
import { Link } from "@/navigation";
import { BlogPost } from "@/types/blog";
import { PaginationMeta } from "@/types/common";
import { DateTime } from "@/utils/date";
import { createResume } from "@/utils/helpers";

type PostsProps = {
  posts?: BlogPost[] | null | undefined;
  meta?: PaginationMeta | null | undefined;
};

export const Posts = ({ posts, meta }: PostsProps) => {
  const t = useTranslations();
  const locale = useLocale();

  return (
    <Container className="px-5">
      <Row className="gx-5 blog-posts">
        {meta?.total === 0 ? (
          <Col lg={8}>
            <div className="alert alert-warning">{t("blog.noPosts")}</div>
          </Col>
        ) : (
          posts?.map((post) => {
            return (
              <Col key={post.id} md={6} xl={4} className="mb-5">
                <Card className="post-preview post-preview-featured lift mb-5 overflow-hidden">
                  {post.coverImage && (
                    <Link href={`/blog/post/${post.slug}`}>
                      <Image
                        src={post.coverImage}
                        width={660}
                        height={360}
                        className="card-img-top"
                        alt={post.title}
                      />
                    </Link>
                  )}
                  <CardBody>
                    <Link href={`/blog/post/${post.slug}`}>
                      <h5 className="card-title">{capitalize(post.title)}</h5>
                      <p className="card-text text-justify">
                        {createResume(post.resume)}
                      </p>
                    </Link>
                  </CardBody>
                  <CardFooter>
                    <div className="post-preview-meta">
                      <Image
                        width={300}
                        height={300}
                        className="post-preview-meta-img"
                        src={
                          post.author?.image ?? "/assets/images/user/user.jpg"
                        }
                        alt={post.author?.name ?? ""}
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
                    <Row className="gx-5 pt-3 post-preview-meta-data">
                      <Col
                        md={6}
                        sm={12}
                        className="post-preview-meta-categories"
                      >
                        <h6>{t("blog.categories.title")}:</h6>
                        {post.categories?.map((category, i) => (
                          <Fragment key={category.id}>
                            <Link href={`/blog/category/${category.slug}/1`}>
                              {category.name}
                            </Link>
                            {i < (post.categories?.length ?? 1) - 1 && (
                              <>,&nbsp;</>
                            )}
                          </Fragment>
                        ))}
                      </Col>
                      <Col md={6} sm={12} className="post-preview-meta-tags">
                        <h6>{t("blog.tags.title")}:</h6>
                        {post.tags?.map((tag, i) => (
                          <Fragment key={tag.id}>
                            <Link key={tag.id} href={`/blog/tag/${tag.slug}/1`}>
                              {tag.name}
                            </Link>
                            {i < (post.tags?.length ?? 1) - 1 && <>,&nbsp;</>}
                          </Fragment>
                        ))}
                      </Col>
                    </Row>
                  </CardFooter>
                </Card>
              </Col>
            );
          })
        )}
      </Row>
      <Pagination meta={meta} />
    </Container>
  );
};
