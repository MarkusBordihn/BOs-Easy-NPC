/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.client.screen.components;

import de.markusbordihn.easynpc.Constants;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Tooltip;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;

public class MultiStateToggleButton extends CustomButton {

  public static final int MIN_STATES = 2;
  public static final int MAX_STATES = 9;
  public static final int DEFAULT_WIDTH = 18;
  public static final int DEFAULT_HEIGHT = 18;
  public static final int DEFAULT_SPRITE_WIDTH = 12;
  public static final int DEFAULT_SPRITE_HEIGHT = 12;
  public static final int DEFAULT_SPRITE_X = 3;
  public static final int DEFAULT_SPRITE_Y = 3;
  public static final ResourceLocation DEFAULT_SPRITE = Constants.TEXTURE_CONFIGURATION;

  private final ToggleState[] states;
  private final ResourceLocation spriteTexture;
  private final int spriteX;
  private final int spriteY;
  private final int spriteWidth;
  private final int spriteHeight;
  private final OnStateChange onStateChange;
  private int currentStateIndex;
  private boolean renderBackground = true;

  public MultiStateToggleButton(
      int left, int top, ToggleState[] states, int initialStateIndex, OnStateChange onStateChange) {
    this(
        left,
        top,
        DEFAULT_WIDTH,
        DEFAULT_HEIGHT,
        DEFAULT_SPRITE,
        DEFAULT_SPRITE_X,
        DEFAULT_SPRITE_Y,
        DEFAULT_SPRITE_WIDTH,
        DEFAULT_SPRITE_HEIGHT,
        states,
        initialStateIndex,
        onStateChange);
  }

  public MultiStateToggleButton(
      int left,
      int top,
      int width,
      int height,
      ToggleState[] states,
      int initialStateIndex,
      OnStateChange onStateChange) {
    this(
        left,
        top,
        width,
        height,
        DEFAULT_SPRITE,
        DEFAULT_SPRITE_X,
        height <= 16 ? 2 : DEFAULT_SPRITE_Y,
        DEFAULT_SPRITE_WIDTH,
        DEFAULT_SPRITE_HEIGHT,
        states,
        initialStateIndex,
        onStateChange);
  }

  public MultiStateToggleButton(
      int left,
      int top,
      int width,
      int height,
      ResourceLocation spriteTexture,
      int spriteX,
      int spriteY,
      int spriteWidth,
      int spriteHeight,
      ToggleState[] states,
      int initialStateIndex,
      OnStateChange onStateChange) {
    super(left, top, width, height, Component.empty(), button -> {});

    if (states.length < MIN_STATES || states.length > MAX_STATES) {
      throw new IllegalArgumentException(
          "MultiStateToggleButton requires between "
              + MIN_STATES
              + " and "
              + MAX_STATES
              + " states, but got "
              + states.length);
    }

    if (initialStateIndex < 0 || initialStateIndex >= states.length) {
      throw new IllegalArgumentException(
          "Initial state index "
              + initialStateIndex
              + " is out of bounds for "
              + states.length
              + " states");
    }

    this.states = states;
    this.currentStateIndex = initialStateIndex;
    this.onStateChange = onStateChange;
    this.spriteTexture = spriteTexture;
    this.spriteX = spriteX;
    this.spriteY = spriteY;
    this.spriteWidth = spriteWidth;
    this.spriteHeight = spriteHeight;
    updateTooltip();
  }

  @Override
  public boolean mouseClicked(double x, double y, int button) {
    if (!this.visible) {
      return false;
    }
    if (this.isValidClickButton(button) && this.clicked(x, y)) {
      this.playDownSound(Minecraft.getInstance().getSoundManager());
      cycleToNextState();
      this.onClick(x, y);
      return true;
    }
    return false;
  }

  @Override
  public void renderButton(GuiGraphics guiGraphics, int mouseX, int mouseY, float partialTicks) {
    if (this.renderBackground) {
      super.renderButton(guiGraphics, mouseX, mouseY, partialTicks);
    }

    // Render the sprite for the current state
    ToggleState currentState = this.states[this.currentStateIndex];
    guiGraphics.blit(
        this.spriteTexture,
        this.getX() + this.spriteX,
        this.getY() + this.spriteY,
        currentState.spriteOffsetX(),
        this.active
            ? currentState.spriteOffsetY()
            : currentState.spriteOffsetY() + this.spriteHeight,
        this.spriteWidth,
        this.spriteHeight,
        256,
        256);
  }

  private void cycleToNextState() {
    this.currentStateIndex = (this.currentStateIndex + 1) % this.states.length;
    updateTooltip();
    if (this.onStateChange != null) {
      this.onStateChange.onStateChange(this, this.currentStateIndex);
    }
  }

  private void updateTooltip() {
    ToggleState currentState = this.states[this.currentStateIndex];
    if (currentState.tooltip() != null) {
      this.setTooltip(Tooltip.create(currentState.tooltip()));
    }
  }

  public int getCurrentStateIndex() {
    return this.currentStateIndex;
  }

  public void setCurrentStateIndex(int stateIndex) {
    if (stateIndex >= 0 && stateIndex < this.states.length) {
      this.currentStateIndex = stateIndex;
      updateTooltip();
    }
  }

  public ToggleState getCurrentState() {
    return this.states[this.currentStateIndex];
  }

  public void setRenderBackground(boolean renderBackground) {
    this.renderBackground = renderBackground;
  }

  @Override
  protected boolean clicked(double x, double y) {
    return this.visible
        && x >= this.getX()
        && y >= this.getY()
        && x < (this.getX() + this.width)
        && y < (this.getY() + this.height);
  }

  @Override
  public boolean isMouseOver(double x, double y) {
    return this.visible
        && x >= this.getX()
        && y >= this.getY()
        && x < (this.getX() + this.width)
        && y < (this.getY() + this.height);
  }

  @FunctionalInterface
  public interface OnStateChange {
    void onStateChange(MultiStateToggleButton button, int newStateIndex);
  }

  public record ToggleState(int spriteOffsetX, int spriteOffsetY, Component tooltip) {}
}
