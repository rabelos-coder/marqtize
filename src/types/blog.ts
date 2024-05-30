import { PaginatedObject } from './common'
import { User } from './user'

export type BlogPost = {
  id: number
  title: string
  slug: string
  resume: string
  content: string
  authorId: number
  coverImage?: string
  isPublished: boolean
  publishedAt?: string
  createdAt: string
  updatedAt: string
  deletedAt?: string
  author?: User
  tags?: BlogTag[]
  categories?: BlogCategory[]
}

export type BlogCategory = {
  id: number
  name: string
  slug: string
  createdAt: string
  updatedAt: string
  deletedAt?: string
  posts?: BlogPost[]
}

export type BlogTag = {
  id: number
  name: string
  slug: string
  createdAt: string
  updatedAt: string
  deletedAt?: string
  posts?: BlogPost[]
}

export type FindFirstBlogTag = {
  findFirstBlogTag: BlogTag
}

export type FindManyBlogTag = {
  findManyBlogTag: BlogTag[]
}

export type FindFirstBlogCategory = {
  findFirstBlogCategory: BlogCategory
}

export type FindManyBlogCategory = {
  findManyBlogCategory: BlogCategory[]
}

export type FindManyBlogPost = {
  findManyBlogPost: BlogPost[]
}

export type FindFirstBlogPost = {
  findFirstBlogPost: BlogPost
}

export type PaginatedBlogPost = {
  paginatedBlogPost: PaginatedObject<BlogPost>
}
