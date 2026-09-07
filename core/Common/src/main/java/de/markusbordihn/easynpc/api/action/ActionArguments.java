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

package de.markusbordihn.easynpc.api.action;

import java.util.List;
import net.minecraft.resources.ResourceLocation;

public record ActionArguments(List<String> values) {

  public static final ActionArguments EMPTY = new ActionArguments(List.of());

  public ActionArguments {
    values = values != null ? List.copyOf(values) : List.of();
  }

  public static ActionArguments of(List<String> arguments) {
    return arguments == null || arguments.isEmpty() ? EMPTY : new ActionArguments(arguments);
  }

  public String getString(int index) {
    return index >= 0 && index < this.values.size() ? this.values.get(index) : null;
  }

  public int getInt(int index, int fallback) {
    String value = this.getString(index);
    if (value == null) {
      return fallback;
    }

    try {
      return Integer.parseInt(value.trim());
    } catch (NumberFormatException e) {
      return fallback;
    }
  }

  public ResourceLocation getIdentifier(int index) {
    String value = this.getString(index);
    return value != null ? ResourceLocation.tryParse(value.trim()) : null;
  }

  public int size() {
    return this.values.size();
  }
}
