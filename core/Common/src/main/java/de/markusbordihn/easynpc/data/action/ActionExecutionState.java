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

package de.markusbordihn.easynpc.data.action;

import net.minecraft.nbt.CompoundTag;

public record ActionExecutionState(
    boolean anyRegularFired, boolean hasScreenAction, ActionDataEntry deferredCloseDialogAction) {

  public static final ActionExecutionState EMPTY = new ActionExecutionState(false, false, null);

  private static final String DATA_ANY_REGULAR_FIRED_TAG = "Fired";
  private static final String DATA_HAS_SCREEN_ACTION_TAG = "Screen";
  private static final String DATA_DEFERRED_CLOSE_DIALOG_TAG = "Close";

  public static ActionExecutionState fromTag(CompoundTag compoundTag) {
    if (compoundTag == null) {
      return EMPTY;
    }

    return new ActionExecutionState(
        compoundTag.getBooleanOr(DATA_ANY_REGULAR_FIRED_TAG, false),
        compoundTag.getBooleanOr(DATA_HAS_SCREEN_ACTION_TAG, false),
        compoundTag
            .getCompound(DATA_DEFERRED_CLOSE_DIALOG_TAG)
            .map(ActionDataEntry::new)
            .orElse(null));
  }

  public ActionExecutionState withAnyRegularFired() {
    return new ActionExecutionState(true, this.hasScreenAction, this.deferredCloseDialogAction);
  }

  public ActionExecutionState withScreenAction() {
    return new ActionExecutionState(this.anyRegularFired, true, this.deferredCloseDialogAction);
  }

  public ActionExecutionState withDeferredCloseDialogAction(ActionDataEntry closeDialogAction) {
    return new ActionExecutionState(this.anyRegularFired, this.hasScreenAction, closeDialogAction);
  }

  public boolean hasDeferredCloseDialogAction() {
    return this.deferredCloseDialogAction != null;
  }

  public CompoundTag createTag() {
    CompoundTag compoundTag = new CompoundTag();
    if (this.anyRegularFired) {
      compoundTag.putBoolean(DATA_ANY_REGULAR_FIRED_TAG, true);
    }

    if (this.hasScreenAction) {
      compoundTag.putBoolean(DATA_HAS_SCREEN_ACTION_TAG, true);
    }

    if (this.deferredCloseDialogAction != null) {
      compoundTag.put(DATA_DEFERRED_CLOSE_DIALOG_TAG, this.deferredCloseDialogAction.createTag());
    }

    return compoundTag;
  }
}
