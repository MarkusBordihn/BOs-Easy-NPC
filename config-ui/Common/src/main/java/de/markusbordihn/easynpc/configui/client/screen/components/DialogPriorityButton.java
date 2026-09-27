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

package de.markusbordihn.easynpc.configui.client.screen.components;

import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.data.dialog.DialogPriority;
import java.util.LinkedHashSet;
import java.util.Set;

public class DialogPriorityButton extends SpinButton<DialogPriorityButton.PriorityValue> {

  public DialogPriorityButton(
      int x, int y, int width, int height, int initialPriority, OnChange<PriorityValue> onChange) {
    super(
        x, y, width, height, getPresetPriorities(), findPresetOrCustom(initialPriority), onChange);
  }

  private static Set<PriorityValue> getPresetPriorities() {
    Set<PriorityValue> priorities = new LinkedHashSet<>();
    priorities.add(new PriorityValue(DialogPriority.MANUAL_ONLY));
    priorities.add(new PriorityValue(DialogPriority.FALLBACK));
    priorities.add(new PriorityValue(DialogPriority.LOW));
    priorities.add(new PriorityValue(DialogPriority.NORMAL));
    priorities.add(new PriorityValue(DialogPriority.HIGH));
    priorities.add(new PriorityValue(DialogPriority.CRITICAL));
    priorities.add(new PriorityValue(Integer.MAX_VALUE, true));
    return priorities;
  }

  private static PriorityValue findPresetOrCustom(int priority) {
    for (PriorityValue preset : getPresetPriorities()) {
      if (!preset.isCustom && preset.value == priority) {
        return preset;
      }
    }

    return new PriorityValue(priority, true);
  }

  public int getPriority() {
    PriorityValue current = this.get();
    return current != null ? current.value : DialogPriority.FALLBACK;
  }

  public boolean isCustom() {
    PriorityValue current = this.get();
    return current != null && current.isCustom;
  }

  public record PriorityValue(int value, boolean isCustom) {
    public PriorityValue(int value) {
      this(value, false);
    }

    @Override
    public String toString() {
      return this.isCustom ? "Custom" : DialogPriority.getDisplayName(this.value);
    }

    @Override
    public boolean equals(Object object) {
      if (this == object) {
        return true;
      }

      if (!(object instanceof PriorityValue other)) {
        return false;
      }

      if (this.isCustom && other.isCustom) {
        return true;
      }

      return !this.isCustom && !other.isCustom && this.value == other.value;
    }

    @Override
    public int hashCode() {
      if (this.isCustom) {
        return Boolean.hashCode(true);
      }

      return Integer.hashCode(this.value);
    }
  }
}
