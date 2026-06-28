/*
 * Copyright 2026 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.configui.client.screen.components;

import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.input.CharacterEvent;
import net.minecraft.client.input.KeyEvent;
import net.minecraft.client.input.MouseButtonEvent;
import net.minecraft.util.Mth;
import org.lwjgl.glfw.GLFW;

public abstract class Popup {

  private static final int RIGHT_MARGIN = 10;

  private boolean visible = false;
  private int x;
  private int y;

  protected static boolean contains(
      double pointX, double pointY, int left, int top, int width, int height) {
    return pointX >= left && pointX < left + width && pointY >= top && pointY < top + height;
  }

  protected abstract int getPanelWidth();

  protected abstract int getPanelHeight();

  protected abstract void renderContent(
      GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float partialTicks);

  protected void onOpen() {}

  protected void onClose() {}

  protected void onMouseClicked(MouseButtonEvent mouseButtonEvent, boolean doubleClick) {}

  protected void onKeyPressed(KeyEvent keyEvent) {}

  protected void onCharTyped(CharacterEvent characterEvent) {}

  protected int getX() {
    return this.x;
  }

  protected int getY() {
    return this.y;
  }

  public boolean isVisible() {
    return this.visible;
  }

  public boolean isMouseOver(double mouseX, double mouseY) {
    return this.visible
        && contains(mouseX, mouseY, this.x, this.y, this.getPanelWidth(), this.getPanelHeight());
  }

  public void open(int anchorX, int anchorY, int screenWidth, int screenHeight) {
    this.x = Mth.clamp(anchorX, 0, Math.max(0, screenWidth - this.getPanelWidth() - RIGHT_MARGIN));
    this.y = Mth.clamp(anchorY, 0, Math.max(0, screenHeight - this.getPanelHeight()));
    this.visible = true;
    this.onOpen();
  }

  public void close() {
    this.visible = false;
    this.onClose();
  }

  public void render(GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float partialTicks) {
    if (!this.visible) {
      return;
    }

    DrawBoxWithOuterBorder.draw(
        guiGraphics, this.x, this.y, this.getPanelWidth(), this.getPanelHeight());
    this.renderContent(guiGraphics, mouseX, mouseY, partialTicks);
  }

  public boolean mouseClicked(MouseButtonEvent mouseButtonEvent, boolean doubleClick) {
    if (!this.visible) {
      return false;
    }
    if (!contains(
        mouseButtonEvent.x(),
        mouseButtonEvent.y(),
        this.x,
        this.y,
        this.getPanelWidth(),
        this.getPanelHeight())) {
      this.close();
      return true;
    }
    this.onMouseClicked(mouseButtonEvent, doubleClick);
    return true;
  }

  public boolean keyPressed(KeyEvent keyEvent) {
    if (!this.visible) {
      return false;
    }

    if (keyEvent.input() == GLFW.GLFW_KEY_ESCAPE) {
      this.close();
      return true;
    }

    this.onKeyPressed(keyEvent);
    return true;
  }

  public boolean charTyped(CharacterEvent characterEvent) {
    if (!this.visible) {
      return false;
    }

    this.onCharTyped(characterEvent);
    return true;
  }
}
