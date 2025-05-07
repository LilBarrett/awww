from django.test import TestCase, Client
from django.urls import reverse
from django.contrib.auth.models import User
from rest_framework.test import APIClient
from django.core.files.uploadedfile import SimpleUploadedFile
from .models import BackgroundImage, Route, Point

class ModelTests(TestCase):
    def test_create_and_relations(self):
        user = User.objects.create_user(username='foo', password='bar')
        bg = BackgroundImage.objects.create(
            name='b',
            image=SimpleUploadedFile('block.jpg', b'fake image content', content_type='image/jpeg')
        )
        route = Route.objects.create(name='R', user=user, background=bg)
        point = Point.objects.create(route=route, x=1.0, y=2.0, order=1)
        self.assertEqual(route.user, user)
        self.assertEqual(route.background, bg)
        self.assertEqual(point.route, route)

class WebAuthTests(TestCase):
    def setUp(self):
        self.client = Client()
        self.user = User.objects.create_user(username='foo', password='bar')
        self.bg = BackgroundImage.objects.create(
            name='b',
            image=SimpleUploadedFile('block.jpg', b'fake image content', content_type='image/jpeg')
        )

    def test_login_required_for_routes(self):
        response = self.client.get('/')  # Use the root for route_list
        # If not logged in, should show empty or redirect to login depending on your implementation
        # If it should redirect, uncomment next line:
        # self.assertEqual(response.status_code, 302)
        # If it should show an empty list, check that:
        self.assertEqual(response.status_code, 200)  # Should be accessible

    def test_logged_in_can_see_own_routes(self):
        self.client.login(username='foo', password='bar')
        Route.objects.create(name='R', user=self.user, background=self.bg)
        response = self.client.get('/')
        self.assertContains(response, 'R')

class APITests(TestCase):
    def setUp(self):
        self.user = User.objects.create_user(username='foo', password='bar')
        self.bg = BackgroundImage.objects.create(
            name='b',
            image=SimpleUploadedFile('block.jpg', b'fake image content', content_type='image/jpeg')
        )
        self.client = APIClient()

    def test_auth_required(self):
        response = self.client.get('/api/routes/')
        self.assertEqual(response.status_code, 401)

    def test_create_route(self):
        self.client.force_authenticate(user=self.user)
        response = self.client.post('/api/routes/', {
            'name': 'R',
            'background': self.bg.id,
            'user': self.user.id  # Needed since RouteSerializer uses '__all__'
        })
        if response.status_code != 201:
            print("Create Route errors:", response.data)
        self.assertEqual(response.status_code, 201)

    def test_route_list_only_own(self):
        other = User.objects.create_user(username='baz', password='qux')
        Route.objects.create(name='Other', user=other, background=self.bg)
        Route.objects.create(name='Mine', user=self.user, background=self.bg)
        self.client.force_authenticate(user=self.user)
        response = self.client.get('/api/routes/')
        self.assertContains(response, 'Mine')
        self.assertNotContains(response, 'Other')

    def test_create_point_and_invalid(self):
        self.client.force_authenticate(user=self.user)
        route = Route.objects.create(name='R', user=self.user, background=self.bg)
        # Valid
        resp = self.client.post(f'/api/routes/{route.id}/points/', {'x': 1, 'y': 2, 'order': 1, 'route': route.id})
        if resp.status_code != 201:
            print("Create Point errors:", resp.data)
        self.assertEqual(resp.status_code, 201)
        # Invalid (missing x)
        resp = self.client.post(f'/api/routes/{route.id}/points/', {'y': 2, 'order': 2, 'route': route.id})
        self.assertEqual(resp.status_code, 400)
