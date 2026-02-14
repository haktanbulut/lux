/*
 * Raylib wrapper for Lux
 *
 * Bridges Lux types (i64, f64) to raylib types (int, float, structs).
 * Raylib passes Color, Vector3, Camera3D by value — Lux can't do that
 * through the FFI, so this wrapper exposes flat-argument functions.
 */

#include <raylib.h>
#include <rlgl.h>
#include <stdint.h>
#include <math.h>

static Camera3D camera;

/* ===== Window ===== */

void lux_init_window(int64_t w, int64_t h, const char *title) {
    InitWindow((int)w, (int)h, title);
}

void lux_close_window(void) {
    CloseWindow();
}

int64_t lux_window_should_close(void) {
    return WindowShouldClose() ? 1 : 0;
}

void lux_set_target_fps(int64_t fps) {
    SetTargetFPS((int)fps);
}

/* ===== Drawing ===== */

void lux_begin_drawing(void) {
    BeginDrawing();
}

void lux_end_drawing(void) {
    EndDrawing();
}

void lux_clear_background(int64_t r, int64_t g, int64_t b, int64_t a) {
    ClearBackground((Color){ (unsigned char)r, (unsigned char)g,
                             (unsigned char)b, (unsigned char)a });
}

/* ===== 3D Camera ===== */

void lux_setup_camera(double px, double py, double pz,
                      double tx, double ty, double tz,
                      double fovy) {
    camera.position = (Vector3){ (float)px, (float)py, (float)pz };
    camera.target   = (Vector3){ (float)tx, (float)ty, (float)tz };
    camera.up       = (Vector3){ 0.0f, 1.0f, 0.0f };
    camera.fovy     = (float)fovy;
    camera.projection = CAMERA_PERSPECTIVE;
}

void lux_begin_mode3d(void) {
    BeginMode3D(camera);
}

void lux_end_mode3d(void) {
    EndMode3D();
}

/* ===== 3D Shapes ===== */

void lux_draw_cube(double x, double y, double z,
                   double w, double h, double d,
                   int64_t r, int64_t g, int64_t b, int64_t a) {
    DrawCube((Vector3){ (float)x, (float)y, (float)z },
             (float)w, (float)h, (float)d,
             (Color){ (unsigned char)r, (unsigned char)g,
                      (unsigned char)b, (unsigned char)a });
}

void lux_draw_cube_wires(double x, double y, double z,
                         double w, double h, double d,
                         int64_t r, int64_t g, int64_t b, int64_t a) {
    DrawCubeWires((Vector3){ (float)x, (float)y, (float)z },
                  (float)w, (float)h, (float)d,
                  (Color){ (unsigned char)r, (unsigned char)g,
                           (unsigned char)b, (unsigned char)a });
}

void lux_draw_grid(int64_t slices, double spacing) {
    DrawGrid((int)slices, (float)spacing);
}

/* ===== Matrix transforms (rlgl) ===== */

void lux_push_matrix(void) { rlPushMatrix(); }
void lux_pop_matrix(void)  { rlPopMatrix(); }

void lux_rotatef(double angle, double x, double y, double z) {
    rlRotatef((float)angle, (float)x, (float)y, (float)z);
}

void lux_translatef(double x, double y, double z) {
    rlTranslatef((float)x, (float)y, (float)z);
}

/* ===== Text ===== */

void lux_draw_text(const char *text, int64_t x, int64_t y,
                   int64_t font_size,
                   int64_t r, int64_t g, int64_t b, int64_t a) {
    DrawText(text, (int)x, (int)y, (int)font_size,
             (Color){ (unsigned char)r, (unsigned char)g,
                      (unsigned char)b, (unsigned char)a });
}

void lux_draw_fps(int64_t x, int64_t y) {
    DrawFPS((int)x, (int)y);
}

/* ===== Time ===== */

double lux_get_time(void) {
    return GetTime();
}

double lux_get_frame_time(void) {
    return (double)GetFrameTime();
}

/* ===== Input ===== */

int64_t lux_is_key_down(int64_t key) {
    return IsKeyDown((int)key) ? 1 : 0;
}

int64_t lux_is_key_pressed(int64_t key) {
    return IsKeyPressed((int)key) ? 1 : 0;
}

/* ===== Math ===== */

double lux_sin(double x) { return sin(x); }
double lux_cos(double x) { return cos(x); }
