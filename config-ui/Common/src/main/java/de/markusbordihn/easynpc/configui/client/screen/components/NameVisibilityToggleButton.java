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

package de.markusbordihn.easynpc.configui.client.screen.components;

import de.markusbordihn.easynpc.data.display.NameVisibilityType;
import de.markusbordihn.easynpc.network.components.TextComponent;

public class NameVisibilityToggleButton extends MultiStateToggleButton {

  public static final int SPRITE_OFFSET_X = 92;
  public static final int SPRITE_OFFSET_Y_NEVER = 166;
  public static final int SPRITE_OFFSET_Y_MID = 178;
  public static final int SPRITE_OFFSET_Y_NEAR = 190;
  public static final int SPRITE_OFFSET_Y_MOUSE_OVER = 202;
  public static final int SPRITE_OFFSET_Y_ALWAYS = 154;

  private NameVisibilityType currentVisibilityType;

  public NameVisibilityToggleButton(
      int left, int top, NameVisibilityType initialType, OnVisibilityChange onVisibilityChange) {
    this(left, top, DEFAULT_WIDTH, DEFAULT_HEIGHT, initialType, onVisibilityChange);
  }

  public NameVisibilityToggleButton(
      int left,
      int top,
      int width,
      int height,
      NameVisibilityType initialType,
      OnVisibilityChange onVisibilityChange) {
    super(
        left,
        top,
        width,
        height,
        createVisibilityStates(),
        getStateIndexFromType(initialType),
        (button, newStateIndex) -> {
          NameVisibilityToggleButton nameButton = (NameVisibilityToggleButton) button;
          nameButton.currentVisibilityType = getTypeFromStateIndex(newStateIndex);
          if (onVisibilityChange != null) {
            onVisibilityChange.onVisibilityChange(nameButton, nameButton.currentVisibilityType);
          }
        });

    this.currentVisibilityType = initialType;
  }

  private static ToggleState[] createVisibilityStates() {
    return new ToggleState[] {
      new ToggleState(
          SPRITE_OFFSET_X,
          SPRITE_OFFSET_Y_ALWAYS,
          TextComponent.getTranslatedConfigText("name_visibility.always")),
      new ToggleState(
          SPRITE_OFFSET_X,
          SPRITE_OFFSET_Y_MID,
          TextComponent.getTranslatedConfigText("name_visibility.mid")),
      new ToggleState(
          SPRITE_OFFSET_X,
          SPRITE_OFFSET_Y_NEAR,
          TextComponent.getTranslatedConfigText("name_visibility.near")),
      new ToggleState(
          SPRITE_OFFSET_X,
          SPRITE_OFFSET_Y_MOUSE_OVER,
          TextComponent.getTranslatedConfigText("name_visibility.mouse_over")),
      new ToggleState(
          SPRITE_OFFSET_X,
          SPRITE_OFFSET_Y_NEVER,
          TextComponent.getTranslatedConfigText("name_visibility.never")),
    };
  }

  private static int getStateIndexFromType(NameVisibilityType type) {
    return switch (type) {
      case ALWAYS -> 0;
      case MID -> 1;
      case NEAR -> 2;
      case MOUSE_OVER -> 3;
      case NEVER -> 4;
    };
  }

  private static NameVisibilityType getTypeFromStateIndex(int stateIndex) {
    return switch (stateIndex) {
      case 0 -> NameVisibilityType.ALWAYS;
      case 1 -> NameVisibilityType.MID;
      case 2 -> NameVisibilityType.NEAR;
      case 3 -> NameVisibilityType.MOUSE_OVER;
      case 4 -> NameVisibilityType.NEVER;
      default -> NameVisibilityType.ALWAYS;
    };
  }

  public NameVisibilityType getVisibilityType() {
    return this.currentVisibilityType;
  }

  public void setVisibilityType(NameVisibilityType type) {
    this.currentVisibilityType = type;
    this.setCurrentStateIndex(getStateIndexFromType(type));
  }

  @FunctionalInterface
  public interface OnVisibilityChange {
    void onVisibilityChange(NameVisibilityToggleButton button, NameVisibilityType newType);
  }
}
