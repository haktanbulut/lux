/*
 * Asteroids game engine for Lux
 *
 * Manages game state (ship, asteroids, bullets) and provides drawing.
 * Lux orchestrates the game loop, input handling, and update scheduling.
 * All numeric params use int64_t / double to match Lux's Int / Float types.
 */

#include <raylib.h>
#include <math.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>

#define MAX_ASTEROIDS 24
#define MAX_BULLETS   20
#define SCREEN_W      800
#define SCREEN_H      600

#define SHIP_ACCEL        200.0
#define SHIP_ROTATE_SPEED 200.0
#define SHIP_DRAG         0.98
#define BULLET_SPEED      400.0
#define BULLET_LIFETIME   2.0
#define SHOOT_COOLDOWN    0.15
#define SPAWN_INTERVAL    2.5

#define ASTEROID_BIG  40.0
#define ASTEROID_MED  20.0
#define ASTEROID_SML  10.0

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/* ===== Data ===== */

typedef struct {
    double x, y, dx, dy, angle;
    int alive;
    double shoot_timer;
} Ship;

typedef struct {
    double x, y, dx, dy, radius;
    int active;
} Asteroid;

typedef struct {
    double x, y, dx, dy, life;
    int active;
} Bullet;

static Ship      ship;
static Asteroid  asteroids[MAX_ASTEROIDS];
static Bullet    bullets[MAX_BULLETS];
static int64_t   score;
static int       game_over;
static double    spawn_timer;
static int       lives;

/* ===== Helpers ===== */

static double deg2rad(double d) { return d * M_PI / 180.0; }

static void wrap(double *x, double *y) {
    if (*x < -50)           *x += SCREEN_W + 100;
    if (*x > SCREEN_W + 50) *x -= SCREEN_W + 100;
    if (*y < -50)           *y += SCREEN_H + 100;
    if (*y > SCREEN_H + 50) *y -= SCREEN_H + 100;
}

static void spawn_asteroid_at(double x, double y, double radius) {
    for (int i = 0; i < MAX_ASTEROIDS; i++) {
        if (!asteroids[i].active) {
            asteroids[i].active = 1;
            asteroids[i].x = x;
            asteroids[i].y = y;
            asteroids[i].radius = radius;
            double a = (double)GetRandomValue(0, 360) * M_PI / 180.0;
            double spd = 40.0 + (double)GetRandomValue(0, 80);
            asteroids[i].dx = cos(a) * spd;
            asteroids[i].dy = sin(a) * spd;
            return;
        }
    }
}

/* ===== API for Lux ===== */

void game_init(void) {
    memset(&ship, 0, sizeof(ship));
    memset(asteroids, 0, sizeof(asteroids));
    memset(bullets, 0, sizeof(bullets));

    ship.x     = SCREEN_W / 2.0;
    ship.y     = SCREEN_H / 2.0;
    ship.angle = -90.0;
    ship.alive = 1;
    score      = 0;
    game_over  = 0;
    spawn_timer = 0;
    lives      = 3;

    /* initial asteroids — away from ship */
    for (int i = 0; i < 5; i++) {
        double ax = (double)GetRandomValue(0, SCREEN_W);
        double ay = (double)GetRandomValue(0, SCREEN_H);
        double dx = ax - ship.x, dy = ay - ship.y;
        if (sqrt(dx*dx + dy*dy) < 120.0) { ax += 160; ay += 160; }
        spawn_asteroid_at(ax, ay, ASTEROID_BIG);
    }
}

void game_reset(void) { game_init(); }

void game_ship_thrust(double dt) {
    if (!ship.alive || game_over) return;
    double r = deg2rad(ship.angle);
    ship.dx += cos(r) * SHIP_ACCEL * dt;
    ship.dy += sin(r) * SHIP_ACCEL * dt;
}

void game_ship_rotate(double dir, double dt) {
    if (!ship.alive || game_over) return;
    ship.angle += dir * SHIP_ROTATE_SPEED * dt;
}

void game_shoot(void) {
    if (!ship.alive || game_over) return;
    if (ship.shoot_timer > 0) return;
    ship.shoot_timer = SHOOT_COOLDOWN;
    for (int i = 0; i < MAX_BULLETS; i++) {
        if (!bullets[i].active) {
            double r = deg2rad(ship.angle);
            bullets[i].active = 1;
            bullets[i].x  = ship.x + cos(r) * 15.0;
            bullets[i].y  = ship.y + sin(r) * 15.0;
            bullets[i].dx = cos(r) * BULLET_SPEED + ship.dx * 0.3;
            bullets[i].dy = sin(r) * BULLET_SPEED + ship.dy * 0.3;
            bullets[i].life = BULLET_LIFETIME;
            return;
        }
    }
}

int64_t game_update_ship(double dt) {
    if (!ship.alive) return 0;
    ship.x += ship.dx * dt;
    ship.y += ship.dy * dt;
    ship.dx *= SHIP_DRAG;
    ship.dy *= SHIP_DRAG;
    ship.shoot_timer -= dt;
    if (ship.shoot_timer < 0) ship.shoot_timer = 0;
    wrap(&ship.x, &ship.y);
    return 0;
}

int64_t game_update_bullets(double dt) {
    int64_t count = 0;
    for (int i = 0; i < MAX_BULLETS; i++) {
        if (!bullets[i].active) continue;
        bullets[i].x += bullets[i].dx * dt;
        bullets[i].y += bullets[i].dy * dt;
        bullets[i].life -= dt;
        if (bullets[i].life <= 0 ||
            bullets[i].x < -20 || bullets[i].x > SCREEN_W + 20 ||
            bullets[i].y < -20 || bullets[i].y > SCREEN_H + 20) {
            bullets[i].active = 0;
            continue;
        }
        count++;
    }
    return count;
}

int64_t game_update_asteroids(double dt) {
    int64_t count = 0;
    for (int i = 0; i < MAX_ASTEROIDS; i++) {
        if (!asteroids[i].active) continue;
        asteroids[i].x += asteroids[i].dx * dt;
        asteroids[i].y += asteroids[i].dy * dt;
        wrap(&asteroids[i].x, &asteroids[i].y);
        count++;
    }
    return count;
}

int64_t game_maybe_spawn(double dt) {
    spawn_timer += dt;
    if (spawn_timer >= SPAWN_INTERVAL) {
        spawn_timer -= SPAWN_INTERVAL;
        int edge = GetRandomValue(0, 3);
        double x, y;
        switch (edge) {
            case 0: x = 0;        y = (double)GetRandomValue(0, SCREEN_H); break;
            case 1: x = SCREEN_W; y = (double)GetRandomValue(0, SCREEN_H); break;
            case 2: x = (double)GetRandomValue(0, SCREEN_W); y = 0;        break;
            default:x = (double)GetRandomValue(0, SCREEN_W); y = SCREEN_H; break;
        }
        spawn_asteroid_at(x, y, ASTEROID_BIG);
        return 1;
    }
    return 0;
}

int64_t game_check_collisions(void) {
    int64_t events = 0;

    /* bullet vs asteroid */
    for (int i = 0; i < MAX_BULLETS; i++) {
        if (!bullets[i].active) continue;
        for (int j = 0; j < MAX_ASTEROIDS; j++) {
            if (!asteroids[j].active) continue;
            double dx = bullets[i].x - asteroids[j].x;
            double dy = bullets[i].y - asteroids[j].y;
            if (sqrt(dx*dx + dy*dy) < asteroids[j].radius) {
                bullets[i].active = 0;
                double r  = asteroids[j].radius;
                double ax = asteroids[j].x;
                double ay = asteroids[j].y;
                asteroids[j].active = 0;
                if (r >= ASTEROID_BIG - 1) {
                    spawn_asteroid_at(ax, ay, ASTEROID_MED);
                    spawn_asteroid_at(ax, ay, ASTEROID_MED);
                    score += 20;
                } else if (r >= ASTEROID_MED - 1) {
                    spawn_asteroid_at(ax, ay, ASTEROID_SML);
                    spawn_asteroid_at(ax, ay, ASTEROID_SML);
                    score += 50;
                } else {
                    score += 100;
                }
                events |= 2;
                break;
            }
        }
    }

    /* ship vs asteroid */
    if (ship.alive && !game_over) {
        for (int j = 0; j < MAX_ASTEROIDS; j++) {
            if (!asteroids[j].active) continue;
            double dx = ship.x - asteroids[j].x;
            double dy = ship.y - asteroids[j].y;
            if (sqrt(dx*dx + dy*dy) < asteroids[j].radius + 10.0) {
                lives--;
                if (lives <= 0) {
                    ship.alive = 0;
                    game_over = 1;
                } else {
                    /* respawn at center */
                    ship.x = SCREEN_W / 2.0;
                    ship.y = SCREEN_H / 2.0;
                    ship.dx = 0; ship.dy = 0;
                }
                events |= 1;
                break;
            }
        }
    }

    return events;
}

int64_t game_get_score(void) { return score; }
int64_t game_is_over(void)   { return game_over ? 1 : 0; }
int64_t game_get_lives(void) { return (int64_t)lives; }

/* ===== Drawing (uses raylib directly) ===== */

void game_draw_ship(void) {
    if (!ship.alive) return;
    double r  = deg2rad(ship.angle);
    double cr = cos(r), sr = sin(r);

    /* triangle ship */
    Vector2 nose  = { (float)(ship.x + cr * 16),           (float)(ship.y + sr * 16) };
    Vector2 left  = { (float)(ship.x + cos(r + 2.4) * 13), (float)(ship.y + sin(r + 2.4) * 13) };
    Vector2 right = { (float)(ship.x + cos(r - 2.4) * 13), (float)(ship.y + sin(r - 2.4) * 13) };

    DrawTriangle(nose, right, left, (Color){220, 220, 255, 255});
    DrawTriangleLines(nose, right, left, WHITE);

    /* thruster flame when moving fast */
    double speed = sqrt(ship.dx * ship.dx + ship.dy * ship.dy);
    if (speed > 30) {
        Vector2 tail = { (float)(ship.x - cr * 12), (float)(ship.y - sr * 12) };
        DrawLineEx(left, tail, 2.0f, (Color){255, 160, 50, 200});
        DrawLineEx(right, tail, 2.0f, (Color){255, 160, 50, 200});
    }
}

void game_draw_asteroids(void) {
    for (int i = 0; i < MAX_ASTEROIDS; i++) {
        if (!asteroids[i].active) continue;
        DrawCircleLines((int)asteroids[i].x, (int)asteroids[i].y,
                        (float)asteroids[i].radius, (Color){180, 180, 180, 255});
    }
}

void game_draw_bullets(void) {
    for (int i = 0; i < MAX_BULLETS; i++) {
        if (!bullets[i].active) continue;
        DrawCircleV((Vector2){(float)bullets[i].x, (float)bullets[i].y},
                    2.5f, (Color){255, 255, 100, 255});
    }
}

void game_draw_hud(void) {
    char buf[64];
    snprintf(buf, sizeof(buf), "Score: %ld", (long)score);
    DrawText(buf, 10, 10, 22, (Color){220, 220, 220, 255});

    /* draw lives as small triangles */
    for (int i = 0; i < lives; i++) {
        float bx = 10.0f + (float)i * 25.0f;
        float by = 42.0f;
        DrawTriangle(
            (Vector2){bx + 6, by},
            (Vector2){bx + 12, by + 14},
            (Vector2){bx, by + 14},
            (Color){180, 180, 220, 255});
    }

    if (game_over) {
        const char *msg = "GAME OVER";
        int w = MeasureText(msg, 44);
        DrawText(msg, SCREEN_W / 2 - w / 2, SCREEN_H / 2 - 35, 44, RED);

        const char *msg2 = "Press R to restart";
        int w2 = MeasureText(msg2, 22);
        DrawText(msg2, SCREEN_W / 2 - w2 / 2, SCREEN_H / 2 + 20, 22,
                 (Color){180, 180, 180, 255});
    }

    DrawFPS(SCREEN_W - 90, 10);
}
