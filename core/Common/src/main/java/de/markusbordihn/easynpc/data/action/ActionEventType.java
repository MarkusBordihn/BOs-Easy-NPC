/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.data.action;

import de.markusbordihn.easynpc.utils.EnumUtils;

public enum ActionEventType {
  NONE,
  ON_BUTTON_CLICK,
  ON_CLOSE_DIALOG,
  ON_DEATH,
  ON_DISTANCE_CLOSE(ActionGroup.DISTANCE_CLOSE, 8.0D),
  ON_DISTANCE_FAR(ActionGroup.DISTANCE_FAR, 32.0D),
  ON_DISTANCE_NEAR(ActionGroup.DISTANCE_NEAR, 16.0D),
  ON_DISTANCE_TOUCH(ActionGroup.DISTANCE_TOUCH, 1.25D),
  ON_DISTANCE_VERY_CLOSE(ActionGroup.DISTANCE_VERY_CLOSE, 4.0D),
  ON_HURT,
  ON_INTERACTION,
  ON_KILL,
  ON_OPEN_DIALOG,
  ON_TRADE;

  private final ActionGroup actionGroup;
  private final double triggerDistance;

  ActionEventType() {
    this(ActionGroup.NONE, 0.0D);
  }

  ActionEventType(ActionGroup actionGroup, double triggerDistance) {
    this.actionGroup = actionGroup;
    this.triggerDistance = triggerDistance;
  }

  public static ActionEventType get(String actionEventType) {
    return EnumUtils.get(ActionEventType.class, actionEventType, NONE);
  }

  public ActionGroup getActionGroup() {
    return this.actionGroup;
  }

  public double getTriggerDistance() {
    return this.triggerDistance;
  }

  public boolean isDistanceEvent() {
    return this.actionGroup != ActionGroup.NONE;
  }
}
