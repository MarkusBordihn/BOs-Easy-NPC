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

import java.util.Arrays;
import java.util.List;
import net.minecraft.resources.Identifier;

public record CustomActionCommand(Identifier actionId, List<String> arguments) {

  public static final CustomActionCommand EMPTY = new CustomActionCommand(null, List.of());

  public static CustomActionCommand parse(String command) {
    if (command == null || command.trim().isEmpty()) {
      return EMPTY;
    }

    String[] parts = command.trim().split("\\s+");
    Identifier actionId = Identifier.tryParse(parts[0]);
    if (actionId == null) {
      return EMPTY;
    }

    return new CustomActionCommand(
        actionId, List.copyOf(Arrays.asList(parts).subList(1, parts.length)));
  }

  public boolean isValid() {
    return this.actionId != null;
  }
}
