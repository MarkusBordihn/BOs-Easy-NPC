package de.markusbordihn.easynpc.client.screen.components;
import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;

import java.util.ArrayList;
import java.util.List;

public class AlignedTextComponent {

    /** Alignment modes for rendering text. */
    public enum Alignment {
        LEFT,
        RIGHT,
        CENTER,
        JUSTIFIED
    }

    /** How to handle words that don't fit in leftover space on the current line. */
    public enum OverflowStrategy {
        /** Move the entire word to the next line (typical word wrapping). */
        WRAP,
        /** Crop (split) the word so part of it stays on this line, and the rest goes on the next line. */
        CROP
    }

    private final Font font = Minecraft.getInstance().font;
    private final List<String> lines = new ArrayList<>();

    private int width;
    private Alignment alignment = Alignment.LEFT;
    private OverflowStrategy overflowStrategy = OverflowStrategy.WRAP;
    private boolean backgroundVisible = true;

    // Smart justification
    private boolean smartJustification = false;
    private int minWordsForJustification = 3;
    private float shortLineThreshold = 0.6f;

    // Clamped justification
    // If maxSpacePerGap >= 0, we won't exceed that spacing between words.
    private int maxSpacePerGap = -1; // -1 => no limit

    /**
     * Sets the text, wrapping it into lines based on the maxWidth.
     * Splits oversized words and uses the chosen overflow strategy.
     *
     * @param text     the text to display
     * @param maxWidth maximum width in pixels
     */
    public void setText(String text, int maxWidth) {
        this.width = maxWidth;
        this.lines.clear();

        // 1) Split raw text by spaces, then further split any oversized words.
        final String[] rawWords = text.split(" ");
        final List<String> finalWords = new ArrayList<>();
        for (final String rawWord : rawWords) {
            if (font.width(rawWord) > maxWidth) {
                finalWords.addAll(splitOversizedWord(rawWord, maxWidth));
            } else {
                finalWords.add(rawWord);
            }
        }

        // 2) Assemble lines from finalWords based on overflowStrategy.
        StringBuilder currentLine = new StringBuilder();
        for (final String word : finalWords) {
            if (currentLine.isEmpty()) {
                currentLine.append(word);
                continue;
            }
            final String potentialLine = currentLine + " " + word;
            if (font.width(potentialLine) <= maxWidth) {
                currentLine.append(" ").append(word);
                continue;
            }
            // Doesn't fit => handle overflow
            if (overflowStrategy == OverflowStrategy.WRAP) {
                lines.add(currentLine.toString());
                currentLine = new StringBuilder(word);
            } else {
                // CROP strategy
                final int leftoverWidth = maxWidth - font.width(currentLine.toString()) - font.width(" ");
                if (leftoverWidth <= 0) {
                    // No space left, just wrap
                    lines.add(currentLine.toString());
                    currentLine = new StringBuilder(word);
                    continue;
                }

                final List<String> splitChunks = splitWordForLeftover(word, leftoverWidth);
                final String fittingChunk = splitChunks.get(0);
                final String remainder = splitChunks.size() > 1 ? splitChunks.get(1) : "";

                currentLine.append(" ").append(fittingChunk);
                lines.add(currentLine.toString());
                currentLine = new StringBuilder();
                if (!remainder.isEmpty()) {
                    if (font.width(remainder) > maxWidth) {
                        // Still oversized => split more
                        final List<String> moreChunks = splitOversizedWord(remainder, maxWidth);
                        if (!moreChunks.isEmpty()) currentLine.append(moreChunks.get(0));
                        for (int i = 1; i < moreChunks.size(); i++) {
                            final String chunk = moreChunks.get(i);
                            if (font.width(currentLine + " " + chunk) <= maxWidth) {
                                currentLine.append(" ").append(chunk);
                            } else {
                                lines.add(currentLine.toString());
                                currentLine = new StringBuilder(chunk);
                            }
                        }
                    } else {
                        currentLine.append(remainder);
                    }
                }
            }
        }
        if (!currentLine.isEmpty()) {
            lines.add(currentLine.toString());
        }
    }

    /**
     * Splits a single word that is already known to exceed maxWidth into smaller chunks.
     */
    private List<String> splitOversizedWord(String word, int maxWidth) {
        final List<String> chunks = new ArrayList<>();
        int start = 0;
        while (start < word.length()) {
            int end = start + 1;
            while (end <= word.length()) {
                final String sub = word.substring(start, end);
                if (font.width(sub) > maxWidth) break;
                end++;
            }
            end--;
            if (end < start) end = start + 1; // force at least 1 char
            chunks.add(word.substring(start, end));
            start = end;
        }
        return chunks;
    }

    /**
     * Splits a word so that part of it fits leftoverWidth, returning [chunk, remainder].
     */
    private List<String> splitWordForLeftover(String word, int leftoverWidth) {
        final List<String> result = new ArrayList<>();
        if (font.width(word) <= leftoverWidth) {
            result.add(word);
            result.add("");
            return result;
        }

        int end = 1;

        while (end <= word.length()) {
            final String sub = word.substring(0, end);
            if (font.width(sub) > leftoverWidth) break;
            end++;
        }

        end--;

        if (end < 1) end = 1;

        final String fitting = word.substring(0, end);
        final String remainder = word.substring(end);
        result.add(fitting);
        result.add(remainder);
        return result;
    }

    /** Sets the alignment mode (LEFT, RIGHT, CENTER, JUSTIFIED). */
    public void setAlignment(Alignment alignment) {
        this.alignment = alignment;
    }

    /** Sets the overflow strategy (WRAP or CROP). */
    public void setOverflowStrategy(OverflowStrategy strategy) {
        this.overflowStrategy = strategy;
    }

    /** Toggles a semi-transparent background behind the text. */
    public void setBackgroundVisible(boolean visible) {
        this.backgroundVisible = visible;
    }

    /**
     * Enables or disables "smart justification."
     * If enabled, lines with too few words or too short lines won't be fully justified.
     *
     * @param enabled            enable/disable
     * @param minWords           min word count to justify
     * @param shortLineRatio     if lineWidth < shortLineRatio * maxWidth => skip justification
     */
    public void setSmartJustification(boolean enabled, int minWords, float shortLineRatio) {
        this.smartJustification = enabled;
        this.minWordsForJustification = minWords;
        this.shortLineThreshold = shortLineRatio;
    }

    /**
     * Sets a maximum spacing (in pixels) for justified lines.
     * If the computed spacing is bigger than this, we "partially justify" instead
     * (the line won't fill the entire width).
     * Set to -1 for no limit (default).
     */
    public void setMaxSpacePerGap(int maxSpace) {
        this.maxSpacePerGap = maxSpace;
    }

    /** Returns the total number of lines (for paging). */
    public int getLineCount() {
        return lines.size();
    }

    /**
     * Renders all lines (no paging) at the given position with the chosen alignment.
     */
    public void renderAll(GuiGraphics guiGraphics, int x, int y, int color) {
        int totalHeight = lines.size() * (font.lineHeight + 2);
        if (backgroundVisible) {
            guiGraphics.fill(x - 2, y - 2, x + width + 2, y + totalHeight + 2, 0x55000000);
        }
        int offsetY = 0;
        for (int i = 0; i < lines.size(); i++) {
            String line = lines.get(i);
            int lineWidth = font.width(line);
            boolean isLastLine = (i == lines.size() - 1);

            if (alignment == Alignment.JUSTIFIED && !isLastLine) {
                if (useJustification(line, lineWidth)) {
                    drawJustifiedLine(guiGraphics, line, x, y + offsetY, width, color);
                    offsetY += font.lineHeight + 2;
                    continue;
                }
            }
            final int drawX = switch (alignment) {
                case RIGHT -> x + (width - lineWidth);
                case CENTER -> x + (width - lineWidth) / 2;
                default -> x; // JUSTIFIED but last line => left align
            };
            drawLine(guiGraphics, line, drawX, y + offsetY, color);
            offsetY += font.lineHeight + 2;
        }
    }

    /**
     * Renders only a subset of lines for a given page index (multi-page dialogs).
     */
    public void renderPage(GuiGraphics guiGraphics, int x, int y, int pageIndex, int maxLines, int color) {
        final int startLine = pageIndex * maxLines;
        final int endLine = Math.min(startLine + maxLines, lines.size());
        final int lineCount = endLine - startLine;
        final int pageHeight = lineCount * (font.lineHeight + 2);

        if (backgroundVisible) {
            guiGraphics.fill(x - 2, y - 2, x + width + 2, y + pageHeight + 2, 0x55000000);
        }

        int offsetY = 0;

        for (int i = startLine; i < endLine; i++) {
            final String line = lines.get(i);
            final int lineWidth = font.width(line);
            final boolean isLastLineOfText = (i == lines.size() - 1);
            final boolean isLastLineOfPage = (i == endLine - 1);

            if (alignment == Alignment.JUSTIFIED && !isLastLineOfText && !isLastLineOfPage) {
                if (useJustification(line, lineWidth)) {
                    drawJustifiedLine(guiGraphics, line, x, y + offsetY, width, color);
                    offsetY += font.lineHeight + 2;
                    continue;
                }
            }

            final int drawX = switch (alignment) {
                case RIGHT -> x + (width - lineWidth);
                case CENTER -> x + (width - lineWidth) / 2;
                default -> x;
            };
            drawLine(guiGraphics, line, drawX, y + offsetY, color);
            offsetY += font.lineHeight + 2;
        }
    }

    /* --------------------------------------------------------------------- */
    /*  Internal Helpers                                                     */
    /* --------------------------------------------------------------------- */

    /**
     * Determines if we should fully justify this line or fall back to left align
     * (smartJustification + min words/threshold check).
     */
    private boolean useJustification(String line, int lineWidth) {
        if (!smartJustification) return true; // no smart check

        final String[] words = line.split(" ");

        if (words.length < minWordsForJustification) return false; // too few words

        return lineWidth >= shortLineThreshold * width; // must be "long enough"
    }

    /**
     * Renders a single line of text at (x, y).
     */
    private void drawLine(GuiGraphics guiGraphics, String text, int x, int y, int color) {
        final PoseStack poseStack = guiGraphics.pose();
        font.drawInBatch(
                text,
                (float) x,
                (float) y,
                color,
                false,
                poseStack.last().pose(),
                guiGraphics.bufferSource(),
                Font.DisplayMode.NORMAL,
                0,
                15728880
        );
    }

    /**
     * Renders a single justified line (distributing extra space between words),
     * respecting maxSpacePerGap if it's >= 0.
     */
    private void drawJustifiedLine(GuiGraphics guiGraphics, String line, int x, int y, int maxWidth, int color) {
        final String[] words = line.split(" ");

        if (words.length <= 1) {
            // Single word => just draw left-aligned
            drawLine(guiGraphics, line, x, y, color);
            return;
        }

        // Calculate total width of text without spaces
        int textWidth = 0;
        for (final String w : words) textWidth += font.width(w);

        final int totalExtraSpace = maxWidth - textWidth;
        if (totalExtraSpace <= 0) {
            // No extra space => just draw left-aligned
            drawLine(guiGraphics, line, x, y, color);
            return;
        }

        final int gaps = words.length - 1;
        int spaceWidth = totalExtraSpace / gaps;
        int extraPixels = totalExtraSpace % gaps;

        // Clamp spacing if maxSpacePerGap is set
        if (maxSpacePerGap >= 0 && spaceWidth > maxSpacePerGap) {
            spaceWidth = maxSpacePerGap;
            extraPixels = 0; // ignore leftover
        }

        int currentX = x;
        for (int i = 0; i < words.length; i++) {
            drawLine(guiGraphics, words[i], currentX, y, color);
            currentX += font.width(words[i]);
            if (i < gaps) {
                currentX += spaceWidth + (i < extraPixels ? 1 : 0);
            }
        }
    }
}